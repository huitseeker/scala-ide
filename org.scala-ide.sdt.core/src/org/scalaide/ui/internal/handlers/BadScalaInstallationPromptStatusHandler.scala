package org.scalaide.ui.internal.handlers

import scala.reflect.runtime.universe
import org.eclipse.core.runtime.IStatus
import org.eclipse.debug.core.IStatusHandler
import org.eclipse.jface.dialogs.IDialogConstants
import org.eclipse.jface.dialogs.{MessageDialog => MD}
import org.scalaide.core.ScalaPlugin
import org.scalaide.core.internal.project.ScalaProject
import org.scalaide.ui.internal.preferences.CompilerSettings
import org.scalaide.util.internal.SettingConverterUtil
import org.scalaide.util.internal.Utils.WithAsInstanceOfOpt
import org.scalaide.util.internal.CompilerUtils.ShortScalaVersion
import scala.tools.nsc.settings.ScalaVersion
import org.scalaide.ui.internal.preferences.CompilerSettings
import org.scalaide.util.internal.ui.UIStatusesConverter
import org.scalaide.core.internal.project.ScalaInstallation
import org.scalaide.core.internal.project.ScalaInstallationChoice
import org.scalaide.util.internal.CompilerUtils
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.scalaide.logging.HasLogger
import org.scalaide.util.internal.Utils
import org.eclipse.ui.internal.dialogs.PropertyDialog
import org.scalaide.util.internal.ui.DisplayThread
import org.eclipse.ui.internal.dialogs.PropertyPageManager

object BadScalaInstallationPromptStatusHandler {

  /** Status code indicating there was a previous scala library detected
   *  on classpath. Linked to BadScalaInstallationPromptStatusHandler
   *  via our statusHandlers extension (see plugin.xml)
   */
  final val STATUS_CODE_PREV_CLASSPATH = 1001

}

class BadScalaInstallationPromptStatusHandler extends RichStatusHandler with HasLogger {

  def doHandleStatus(status: IStatus, source: Object) = {
    val scalaProject = PartialFunction.condOpt(source){case (p: ScalaProject) => p}
    val shell = ScalaPlugin.getShell

    val title = "Wrong Scala library version detected in this project"
    val projectName = scalaProject map ( _.underlying.getName()) getOrElse("")
    val message = status.getMessage()

    if (scalaProject.isDefined) {
      val project = scalaProject.get
      val javaProject = project.javaProject
      val severity = UIStatusesConverter.MessageDialogOfIStatus(status.getSeverity())
      val dialog = new MD(
        shell,
        title,
        null,
        message,
        severity,
        Array(IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL),
        1)
      dialog.open()
      val buttonId = dialog.getReturnCode()
      if (buttonId == IDialogConstants.OK_ID) {
            // This works, but it conflicts with any preexisting Preference page
            // PreferencesUtil.createPropertyDialogOn(shell, javaProject.getProject(), CompilerSettings.PAGE_ID, null, null).open()) )
            val pd = PropertyDialog.createDialogOn(shell, CompilerSettings.PAGE_ID, javaProject.getProject())
            pd.open()
            pd
      }
    }
  }

}
