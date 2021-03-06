package org.scalaide.core.internal.quickassist

import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jface.text.IDocument
import org.scalaide.refactoring.internal.RefactoringExecutor
import org.scalaide.refactoring.internal.RefactoringHandler
import org.scalaide.refactoring.internal.rename.Rename
import org.scalaide.core.quickassist.BasicCompletionProposal

abstract class ProposalRefactoringHandlerAdapter(
    handler: RefactoringHandler,
    displayString: String,
    relevance: Int = RelevanceValues.ProposalRefactoringHandlerAdapter)
  extends BasicCompletionProposal(relevance, displayString) {

  override def apply(document: IDocument): Unit =
    handler.perform()

  def isValidProposal: Boolean = {
    val ra = handler match {
      case r: Rename => r.getRenameRefactoring
      case r: RefactoringExecutor => r
    }
    ra.createScalaIdeRefactoringForCurrentEditorAndSelection exists {
      (refactoring) => !refactoring.checkInitialConditions(new NullProgressMonitor).hasWarning
    }
  }

}
