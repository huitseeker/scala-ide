/*
 */
package org.scalaide.core.internal.compiler

import scala.tools.nsc.doc.html.HtmlPage
import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.reflect.internal.util.SourceFile
import org.eclipse.jface.internal.text.html.BrowserInformationControlInput
import scala.xml.NodeSeq
import scala.beans.BeanProperty
import scala.tools.nsc.interactive.Response
import scala.reflect.api.Position
import org.scalaide.core.compiler.IScalaPresentationCompiler.Implicits._
import org.eclipse.jdt.core.IJavaElement
import scala.reflect.internal.Flags
import org.eclipse.jdt.core.IJavaProject

trait Scaladoc extends MemberLookupBase with CommentFactoryBase { this: ScalaPresentationCompiler =>
  val global: this.type = this

  // @see the corresponding member in
  // src/scaladoc/scala/tools/nsc/doc/ScaladocAnalyzer.scala
  override def chooseLink(links: List[LinkTo]): LinkTo = links.headOption.orNull

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] = {
    assert(onCompilerThread, "!onCompilerThread")
    if (sym.isClass || sym.isModule)
      Some(LinkToTpl(sym))
    else
      if ((site.isClass || site.isModule) && site.info.members.toList.contains(sym))
        Some(LinkToMember(sym, site))
      else None
  }

  override def toString(link: LinkTo): String = {
    assert(onCompilerThread, "!onCompilerThread")
    link match {
      case LinkToMember(mbr: Symbol, site: Symbol) =>
        mbr.signatureString + " in " + site.toString
      case LinkToTpl(sym: Symbol) => sym.toString
      case _ => link.toString
    }
  }

  override def warnNoLink: Boolean = false
  override def findExternalLink(sym: Symbol, name: String): Option[LinkToExternal] = None

  def parsedDocComment(sym: Symbol, site: Symbol, javaProject:IJavaProject): Option[Comment] = {
    val res =

      for (u <- findCompilationUnit(sym, javaProject)) yield u.withSourceFile { (source, _) =>

        def listFragments(syms:List[Symbol]): List[(Symbol, SourceFile)] = syms flatMap ((sym) =>
          findCompilationUnit(sym, javaProject) flatMap {_.withSourceFile { (source, _) => (sym,source)}}
        )

        def withFragments(fragments: List[(Symbol, SourceFile)]): Option[(String, String, Position)] = {
          val response = new Response[(String, String, Position)]
          askDocComment(sym, source, site, fragments, response)
          response.get.left.toOption
        }

        asyncExec {
          if (sym.owner.hasPackageFlag) sym.baseClasses else sym::sym.allOverriddenSymbols:::site.baseClasses
        }.getOption() flatMap { syms =>
          withFragments(listFragments(syms)) flatMap {
            case (expanded, raw, pos) if !expanded.isEmpty =>
              asyncExec{ parseAtSymbol(expanded, raw, pos, site) }.getOption()
            case _ =>
              None
          }
        }
      } getOrElse (None)
    res.flatten
  }

  def headerForSymbol(sym:Symbol, tpe: Type): String = {
    def compose(ss: List[String]): String = ss.filterNot(_.isEmpty).mkString(" ")

    def defString(sym: Symbol, tpe: Type): String = {
      // NoType is returned for defining occurrences, in this case we want to display symbol info itself.
      val tpeinfo = if (tpe ne NoType) tpe.widen else sym.info
      compose(List(sym.flagString(Flags.ExplicitFlags), sym.keyString, sym.varianceString + sym.nameString +
        sym.infoString(tpeinfo)))
    }

    if (sym.isClass || sym.isModule) sym.fullNameString else {
      val tp = sym.tpe.asSeenFrom(tpe.widen, sym.enclClass)
      defString(sym, tp)
    }
  }

  object HtmlProducer {
    def apply(comment: Comment, sym: Symbol, header: String, javaProject: IJavaProject) = {
      val html =
       <html>
          <body>
           { htmlContents(header, comment) }
          </body>
        </html>
      new BrowserInput(html.toString, getJavaElement(sym, javaProject).orNull)
    }

   def bodiesToHtml(caption: String, bodies: List[Body]): NodeSeq =
      bodies match {
        case Nil => NodeSeq.Empty
        case _ =>
          <p>
            {
              val first = if (!caption.isEmpty) <h3>{ caption }</h3> else NodeSeq.Empty
              val last = bodies.flatMap(bodyToHtml).flatten
              first ++ last
            }
          </p>
       }

    def bodyToHtml(body: Body): NodeSeq = body.blocks flatMap blockToHtml
    def blockToHtml(block: comment.Block): NodeSeq = block match {
      case Title(in, _) => <h6>{ inlineToHtml(in) }</h6>
      case Paragraph(in) => <p>{ inlineToHtml(in) }</p>
      case Code(data) => <br/><pre><i>{ scala.xml.Text(data) }</i></pre><br/>
      case UnorderedList(items) => <ul>{ listItemsToHtml(items) }</ul>
      case OrderedList(items, listStyle) => <ol class={ listStyle }>{ listItemsToHtml(items) }</ol>
      case DefinitionList(items) =>
        <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
      case HorizontalRule() => <hr/>
    }

    def listItemsToHtml(items: Seq[comment.Block]) =
      items.foldLeft(NodeSeq.Empty){ (xmlList, item) =>
        item match {
          case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
            xmlList.init ++ <li>{ xmlList.last.child ++ blockToHtml(item) }</li>
          case Paragraph(inline) =>
            xmlList :+ <li>{ inlineToHtml(inline) }</li>  // LIs are blocks, no need to use Ps
          case block =>
            xmlList :+ <li>{ blockToHtml(block) }</li>
        }
    }

    def inlineToHtml(inl: Inline): NodeSeq = inl match {
      case Chain(items) => items flatMap inlineToHtml
      case Italic(in) => <i>{ inlineToHtml(in) }</i>
      case Bold(in) => <b>{ inlineToHtml(in) }</b>
      case Underline(in) => <u>{ inlineToHtml(in) }</u>
      case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
      case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
      case Link(raw, title) => <a href={ raw } target="_blank">{ inlineToHtml(title) }</a>
      case Monospace(in) => <code>{ inlineToHtml(in) }</code>
      case Text(text) => scala.xml.Text(text)
      case Summary(in) => inlineToHtml(in)
      case HtmlTag(tag) => scala.xml.Unparsed(tag)
      case EntityLink(in, _) => inlineToHtml(in)
    }

    def htmlContents(header: String, comment: Comment): NodeSeq = {
      val headerHtml =
        if (header.isEmpty) NodeSeq.Empty
        else
          <h3>{ header }</h3>

      val mainHtml = List(
        bodyToHtml(comment.body),
        bodiesToHtml("Example" + (if (comment.example.length > 1) "s" else ""), comment.example),
        bodiesToHtml("Version", comment.version.toList),
        bodiesToHtml("Since", comment.since.toList),
        bodiesToHtml("Note", comment.note),
        bodiesToHtml("See also", comment.see),
        bodiesToHtml("To do", comment.todo),
        bodiesToHtml("Deprecated", comment.deprecated.toList),
        bodiesToHtml("Exceptions Thrown", comment.throws.toList.sortBy(_._1).map {
          case (name, body) => Body(Title(Text(name), 0) :: body.blocks.toList)
        })
      ).flatten

      headerHtml ++ mainHtml
    }
  }

}

class BrowserInput(@BeanProperty val html: String,
                   @BeanProperty val inputElement: Object,
                   @BeanProperty val inputName: String) extends BrowserInformationControlInput(null) {
  @deprecated("use BrowserInput(html:String, sym:IJavaElement), see ScalaJavaMapper.getJavaElement")
  def this(html: String, sym:Symbol) = this(html, sym, sym.toString)
  def this(html: String, sym: IJavaElement) = this(html, sym, sym.toString)
}