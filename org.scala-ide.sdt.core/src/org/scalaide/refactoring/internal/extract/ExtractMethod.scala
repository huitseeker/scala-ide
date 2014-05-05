package org.scalaide.refactoring.internal.extract

import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.implementations.extraction

import org.scalaide.core.internal.jdt.model.ScalaSourceFile

class ExtractMethod extends ExtractionExecutor {
  def createRefactoring(selectionStart: Int, selectionEnd: Int, file: ScalaSourceFile) =
    new ScalaIdeExtractionRefactoring(selectionStart, selectionEnd, file) {
      val refactoring = file.withSourceFile { (sourceFile, compiler) =>
        new extraction.ExtractMethod with InteractiveScalaCompiler {
          val global = compiler
        }
      }.get
    }
}