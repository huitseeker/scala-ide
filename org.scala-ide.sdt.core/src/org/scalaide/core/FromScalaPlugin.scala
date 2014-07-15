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



  def getJavaProject(project: IProject) = JavaCore.create(project)



}