package org.scalaide.core

import scala.tools.nsc.Settings
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.core.runtime.CoreException

object FromScalaPlugin {

  def defaultScalaSettings(errorFn: String => Unit = Console.println): Settings = new Settings(errorFn)

  import org.scalaide.core.ScalaConstants._
  
  /** Is the file buildable by the Scala plugin? In other words, is it a
   *  Java or Scala source file?
   *
   *  @note If you don't have an IFile yet, prefer the String overload, as
   *        creating an IFile is usually expensive
   */
  def isBuildable(file: IFile): Boolean =
    isBuildable(file.getName())

  /** Is the file buildable by the Scala plugin? In other words, is it a
   *  Java or Scala source file?
   */
  def isBuildable(fileName: String): Boolean =
    (fileName.endsWith(ScalaFileExtn) || fileName.endsWith(JavaFileExtn))
    

  def workspaceRoot = ResourcesPlugin.getWorkspace.getRoot

  def getJavaProject(project: IProject) = JavaCore.create(project)

  def getShell: Shell = getWorkbenchWindow.map(_.getShell).orNull

  def getWorkbenchWindow = {
    val workbench = PlatformUI.getWorkbench
    Option(workbench.getActiveWorkbenchWindow) orElse workbench.getWorkbenchWindows.headOption
  }

  
  /**
   * Return true if the given Java project is also a Scala project, false othrerwise.
   */
  def isScalaProject(project: IJavaProject): Boolean =
    (project ne null) && isScalaProject(project.getProject)

  /**
   * Return true if the given project is a Scala project, false othrerwise.
   */
  def isScalaProject(project: IProject): Boolean =
    try {
      project != null && project.isOpen && project.hasNature(NatureId)
    } catch {
      case _: CoreException => false
    }

}