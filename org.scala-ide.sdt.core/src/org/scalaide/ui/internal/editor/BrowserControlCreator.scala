package org.scalaide.ui.internal.editor

import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.jface.action.ToolBarManager
import org.eclipse.jface.internal.text.html.BrowserInformationControl
import org.eclipse.jface.text.AbstractReusableInformationControlCreator
import org.eclipse.jface.text.DefaultInformationControl
import org.eclipse.jface.text.IInformationControlCreator
import org.eclipse.swt.widgets.Shell

object BrowserControlCreator {
  def apply(): IInformationControlCreator = {
    val inner = new AbstractReusableInformationControlCreator {
      def doCreateInformationControl(parent: Shell) =
        if (BrowserInformationControl.isAvailable(parent))
          new BrowserInformationControl(parent, PreferenceConstants.APPEARANCE_JAVADOC_FONT, null: ToolBarManager)
        else
          new DefaultInformationControl(parent, JavaPlugin.getAdditionalInfoAffordanceString())
    }
    new JavadocHover.HoverControlCreator(inner, true)
  }
}
