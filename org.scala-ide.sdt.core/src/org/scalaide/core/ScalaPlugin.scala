package org.scalaide.core

import org.eclipse.core.resources.IProject
import org.scalaide.core.internal.project.ScalaProject
import scala.collection.mutable
import org.scalaide.logging.HasLogger
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.ui.plugin.AbstractUIPlugin
import scala.tools.nsc.settings.ScalaVersion
import org.scalaide.util.internal.CompilerUtils

object ScalaPlugin {

  def plugin: ScalaPlugin = org.scalaide.core.internal.ScalaPlugin.plugin

}

trait ScalaPlugin extends AbstractUIPlugin with HasLogger {
  
  import ScalaConstants._

  lazy val noTimeoutMode = System.getProperty(NoTimeoutsProperty) ne null
  lazy val headlessMode = System.getProperty(HeadlessProperty) ne null
  
  // runtime Scala
  lazy val scalaVersion = ScalaVersion.current
  lazy val shortScalaVersion = CompilerUtils.shortString(scalaVersion)



  // Scala project instances
  private val projects = new mutable.HashMap[IProject, ScalaProject]

  def getScalaProject(project: IProject): ScalaProject = projects.synchronized {
    projects.get(project) getOrElse {
      val scalaProject = ScalaProject(project)
      projects(project) = scalaProject
      scalaProject
    }
  }

  /**
   * Return Some(ScalaProject) if the project has the Scala nature, None otherwise.
   */
  def asScalaProject(project: IProject): Option[ScalaProject] = {
    if (ScalaProject.isScalaProject(project)) {
      Some(getScalaProject(project))
    } else {
      logger.debug("`%s` is not a Scala Project.".format(project.getName()))
      None
    }
  }



  protected def disposeProject(project: IProject): Unit = {
    projects.synchronized {
      projects.get(project) foreach { (scalaProject) =>
        projects.remove(project)
        scalaProject.dispose()
      }
    }
  }
  
}