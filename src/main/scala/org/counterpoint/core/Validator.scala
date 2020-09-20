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

  // TODO: Validate global rules
  // TODO: Warn about heuristic violations
  // TODO: Read from MusicXML file
  def validate(measures: Vector[Measure]): Evaluation = {
    ???
  }
}
