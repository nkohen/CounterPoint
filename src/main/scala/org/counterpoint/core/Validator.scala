package org.counterpoint.core

object Validator {

  sealed trait Evaluation {
    def map(f: Unit => Unit): Evaluation = this
    def flatMap(f: Unit => Evaluation): Evaluation
  }

  case class Fail(message: String) extends Evaluation {
    override def flatMap(f: Unit => Evaluation): Evaluation = this
  }

  case object Pass extends Evaluation {
    override def flatMap(f: Unit => Evaluation): Evaluation = f(())
  }

  def rule(requirement: Boolean, errorMsg: String): Evaluation = {
    if (requirement) {
      Pass
    } else {
      Fail(errorMsg)
    }
  }

  // TODO: Warn about heuristic violations
  // TODO: Read from MusicXML file
  def validate(measures: Vector[Measure]): Evaluation = {
    measures.zipWithIndex.foreach {
      case (measure, index) =>
        if (index != 0) {
          measure.prevMeasure = measures(index - 1)
        }
        if (index != measures.length - 1) {
          measure.nextMeasure = measures(index + 1)
        }
    }

    val allNotes = measures.foldLeft(Vector.empty[Note]) {
      case (notes, measure) =>
        notes ++ measure.notes
    }
    val halfBeneathTonic = Note(measures.last.givenNote.id - 1)
    val (_, globalRules) =
      allNotes.tail.foldLeft[(Note, Evaluation)]((allNotes.head, Pass)) {
        case ((note1, eval), note2) =>
          val newEval = for {
            _ <- eval
            _ <- rule(note1 != note2,
                      s"No notes ($note1) may be repeated consecutively")
            _ <- rule(
              note1 > note2 || note2 != halfBeneathTonic,
              s"May not leap to half step beneath the tonic: ($note1, $note2)")
          } yield Pass
          (note2, newEval)
      }

    val (_, lastLine, linesWithoutLast) = allNotes.tail.foldLeft(
      (allNotes.head, Vector(allNotes.head), Vector.empty[Line])) {
      case ((note1, currentLine, lines), note2) =>
        if (currentLine.length < 2) {
          (note2, currentLine :+ note2, lines)
        } else {
          val ascending = currentLine(0) < currentLine(1)
          if ((ascending && note1 < note2) || (!ascending && note1 > note2)) {
            (note2, currentLine :+ note2, lines)
          } else {
            (note2, Vector(note1, note2), lines :+ Line(currentLine))
          }
        }
    }
    val lines = linesWithoutLast :+ Line(lastLine)

    for {
      _ <- globalRules
      _ <- measures.foldLeft[Evaluation](Pass) {
        case (eval, measure) =>
          eval.flatMap(_ => measure.validateMeasureRules)
      }
      _ <- lines.foldLeft[Evaluation](Pass) {
        case (eval, line) =>
          eval.flatMap(_ => line.validateLineRules)
      }
    } yield Pass
  }
}
