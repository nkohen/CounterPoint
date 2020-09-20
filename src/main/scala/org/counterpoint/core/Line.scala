package org.counterpoint.core

import org.counterpoint.core.Validator.{rule, Evaluation, Pass}

case class Line(notes: Vector[Note]) extends IndexedSeq[Note] {
  override def iterator: Iterator[Note] = notes.iterator
  override def length: Int = notes.length
  override def apply(idx: Int): Note = notes(idx)

  require(length >= 2, s"Lines must contain at least two notes: $notes")

  def validateLineRules: Evaluation = {
    if (length == 2) {
      val note1 = notes.head
      val note2 = notes.last
      val ascending = note1 < note2
      val descending = note2 < note1
      val octaveLeap = Math.abs(note1.id - note2.id) == 12
      for {
        _ <- rule(!note1.tritoneWith(note2),
                  s"A two note line ($note1, $note2) may not leap a tritone")
        _ <- rule(
          !ascending || octaveLeap || note2.id - note1.id <= 9,
          s"Ascending two note line may leap at most a minor 6th: ($note1, $note2)")
        _ <- rule(
          !descending || octaveLeap || note1.id - note2.id <= 8,
          s"Descending two note line may leap at most a perfect 5th: ($note1, $note2)")
      } yield Pass
    } else {
      val (_, ascending, descending) =
        notes.tail.foldLeft((notes.head, true, true)) {
          case ((note1, asc, desc), note2) =>
            (note2, asc && note1 < note2, desc && note2 < note1)
        }
      lazy val (_, legalIncrements) =
        notes.tail.foldLeft[(Note, Evaluation)]((notes.head, Pass)) {
          case ((note1, eval), note2) =>
            lazy val interval = note1.intervalWith(note2)
            lazy val lowestNotes =
              (descending && (note2 == notes.last)) || (ascending && (note1 == notes.head))
            val newEval = for {
              _ <- eval
              _ <- rule(
                interval == 2 || (lowestNotes && interval == 3 && Note
                  .pitchDiff(note1, note2) < 12),
                s"A leap may only occur between the lowest notes in a line and may only be a third ($note1, $note2)"
              )
            } yield Pass

            (note2, newEval)
        }
      for {
        _ <- rule(
          !notes.head.tritoneWith(notes.last),
          s"The first (${notes.head}) and last (${notes.last}) notes in a line cannot together form a tritone")
        _ <- rule(
          ascending || descending,
          s"A line must consist of ascending or descending notes: $notes")
        _ <- legalIncrements
      } yield Pass
    }
  }
}
