package org.scalaide.ui.internal.actions

import scala.collection.mutable.Set
import org.scalaide.ui.internal.project.ScalaInstallationUIProviders
import org.eclipse.ui.IObjectActionDelegate
import org.eclipse.core.resources.IProject
import scala.collection.mutable.HashSet
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.core.runtime.IAdaptable
import org.scalaide.util.internal.eclipse.EclipseUtils._
import org.eclipse.jface.viewers.IContentProvider
import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.scalaide.core.internal.ScalaPlugin
import org.eclipse.jface.window.Window
import org.scalaide.core.internal.project.ScalaInstallation
import org.scalaide.core.internal.project.ScalaInstallationChoice
import org.scalaide.util.internal.Utils._
import org.scalaide.util.internal.eclipse.SWTUtils
import org.eclipse.ui.IWorkbenchWindow
import org.scalaide.core.IScalaPlugin
import org.scalaide.util.internal.SettingConverterUtil
import org.eclipse.ui.IWorkbenchPart
import org.scalaide.util.internal.CompilerUtils.shortString
import scala.tools.nsc.settings.ScalaVersion

/** Offers to set a Scala Installation (and by consequence project-specific settings) for a
 *  selection (possibly multiple) of Scala Projects
 */
class ScalaInstallationAction extends IObjectActionDelegate with ScalaInstallationUIProviders {
  var parentWindow: IWorkbenchWindow = null
  val currSelected: Set[IProject] = new HashSet[IProject]()
  val scalaPlugin = IScalaPlugin()

  private def selectionObjectToProject(selectionElement: Object): Option[IProject] = selectionElement match {
    case project: IProject => Some(project)
    case adaptable: IAdaptable => adaptable.adaptToSafe[IProject]
    case _ => None
  }

  override def setActivePart(action: IAction, targetpart: IWorkbenchPart) = {}

  def selectionChanged(action: IAction, select: ISelection) = {
    currSelected.clear()
    for {
      selection <- Option(select) collect { case s: IStructuredSelection => s }
      selObject <- selection.toArray
      project <- selectionObjectToProject(selObject)
    } currSelected.add(project)
    if (action != null) {
      action.setEnabled(!currSelected.isEmpty)
    }
  }

  // only to be used on resolvable choices, which is the case here
  private def getDecoration(si:ScalaInstallationChoice): String = si.marker match{
    case Left(version) => s"Latest ${shortString(version)} (dynamic)"
    case Right(_) => s"Fixed Scala Version: ${ScalaInstallation.resolve(si).get.version.toString}"
  }

  def labeler = new org.eclipse.jface.viewers.LabelProvider {
   override def getText(element: Any): String = PartialFunction.condOpt(element){case si: ScalaInstallationChoice => getDecoration(si)}.getOrElse("")
  }

  def run(action: IAction) {
    if (!currSelected.isEmpty) {
      val chosenScalaInstallation = chooseScalaInstallation()
      chosenScalaInstallation foreach { (sic) =>
        currSelected foreach {
          scalaPlugin.asScalaProject(_) foreach { (spj) =>
            spj.storage.setValue(SettingConverterUtil.SCALA_DESIRED_INSTALLATION, sic.toString())
          }
        }
      }
    }
  }

  def itemTitle = "Fixed Scala Installation"
  //Ask the user to select a build configuration from the selected project.
  private def chooseScalaInstallation(): Option[ScalaInstallationChoice] = {
    val dialog = new ElementListSelectionDialog(getShell(), labeler) {
      def getInstallationChoice(): Option[ScalaInstallationChoice] = {
        val res = getResult()
        if (res != null && !res.isEmpty) res(0).asInstanceOfOpt[ScalaInstallationChoice]
        else None
      }
    }
    val dynamicVersions:List[ScalaInstallationChoice] = List("2.10", "2.11").map((s) => ScalaInstallationChoice(ScalaVersion(s)))
    val fixedVersions: List[ScalaInstallationChoice] = ScalaInstallation.availableInstallations.map((si) => ScalaInstallationChoice(si))
    dialog.setElements((dynamicVersions ++ fixedVersions).toArray)
    dialog.setTitle("Scala Installation Choice")
    dialog.setMessage("Select a Scala Installation for your projects")
    dialog.setMultipleSelection(false)
    val result = dialog.open()
    labeler.dispose()
    if (result == Window.OK) {
      dialog.getInstallationChoice()
    } else None
  }

  private def getShell() = if (parentWindow == null) SWTUtils.getShell else parentWindow.getShell

  def init(window: IWorkbenchWindow) {
    parentWindow = window
  }

}