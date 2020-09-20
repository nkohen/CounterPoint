package org.counterpoint.core

import org.counterpoint.core.Validator.{rule, Evaluation, Pass}

sealed trait Measure {
  def givenNote: Note
  def notes: Vector[Note]
  def validateMeasureRules: Evaluation
  var prevMeasure: Measure = _
  var nextMeasure: Measure = _
}

object Measure {

  def isContraryMotion(measure1: Measure, measure2: Measure): Boolean = {
    val noteDiff = measure1.notes.last.id - measure2.notes.head.id
    val givenDiff = measure1.givenNote.id - measure2.givenNote.id

    noteDiff * givenDiff < 0
  }

  def validateFirstNote(measure: Measure, prevMeasure: Measure): Evaluation = {
    val note = measure.notes.head
    val givenNote = measure.givenNote
    for {
      _ <- rule(
        note consonantWith givenNote,
        s"The first note of a measure ($note) must be consonant with the given note ($givenNote)")
      _ <- rule(
        !Vector(1, 5, 8).contains(note intervalWith givenNote) || Measure
          .isContraryMotion(prevMeasure, measure),
        s"If the first note ($note) is a 1 or 5 or 8 interval from the given note ($givenNote), contrary motion is required"
      )
    } yield Pass
  }
}

sealed trait WholeNoteMeasure extends Measure {
  def note: Note

  override def notes: Vector[Note] = Vector(note)
}

sealed trait MultiNoteMeasure extends Measure {
  def note3: Note
  def note4: Note

  def validateMultiNoteMeasureRules: Evaluation = {
    val nextNotes = if (nextMeasure.notes.length == 4) {
      Vector(nextMeasure.notes(0), nextMeasure.notes(1), nextMeasure.notes(2))
    } else {
      Vector.fill(3)(nextMeasure.notes.head)
    }

    for {
      _ <- rule(
        note3 consonantWith givenNote,
        s"The third note of a measure ($note3) must be consonant with the given note ($givenNote)")
      _ <- rule(
        (note4 consonantWith givenNote) || (nextMeasure.notes.length == 4 && note4
          .legalDissonantWith(givenNote,
                              note3,
                              nextNotes(0),
                              nextNotes(1),
                              nextNotes(2))),
        s"The fourth note of a measure ($note4) must either be consonant or legally dissonant with the given note ($givenNote)"
      )
      _ <- rule(
        !note4.isPeak(note3, nextMeasure.notes.head),
        s"The fourth note of a measure ($note4) cannot be a high note (between $note3 and ${nextMeasure.notes.head})"
      )
    } yield Pass
  }
}

sealed trait FourNoteMeasure extends MultiNoteMeasure {
  def note1: Note
  def note2: Note

  override def notes: Vector[Note] = Vector(note1, note2, note3, note4)

  def validateFourNoteMeasureRules: Evaluation = {
    for {
      _ <- validateMultiNoteMeasureRules
      _ <- rule(
        (note2 consonantWith givenNote) || note2.legalDissonantWith(
          givenNote,
          note1,
          note3,
          note4,
          nextMeasure.notes.head),
        s"The second note of a measure ($note2) must either be consonant or legally dissonant with the given note ($givenNote)"
      )
      _ <- rule(
        !note2.isPeak(note1, note3),
        s"The second of a measure note ($note2) cannot be a high note (between $note1 and $note3)")
    } yield Pass
  }
}

case class NormalMeasure(
    note1: Note,
    note2: Note,
    note3: Note,
    note4: Note,
    givenNote: Note)
    extends FourNoteMeasure {

  def validateMeasureRules: Evaluation = {
    for {
      _ <- rule(
        note1 != givenNote,
        s"The first note of a measure ($note1) cannot be the same as the given note ($givenNote)")
      _ <- Measure.validateFirstNote(this, prevMeasure)
      _ <- validateFourNoteMeasureRules
    } yield Pass
  }
}

object NormalMeasure {

  def apply(notes: Vector[Note], givenNote: Note): NormalMeasure = {
    NormalMeasure(notes(0), notes(1), notes(2), notes(3), givenNote)
  }
}

sealed trait FirstMeasure extends Measure

case class NormalFirstMeasure(
    note1: Note,
    note2: Note,
    note3: Note,
    note4: Note,
    givenNote: Note)
    extends FirstMeasure
    with FourNoteMeasure {

  override def validateMeasureRules: Evaluation = {
    for {
      _ <- rule(
        Vector(1, 5, 8).contains(note1 intervalWith givenNote),
        s"The first note ($note1) must be an interval of 1, 5, or 8 with the given note ($givenNote)")
      _ <- rule(
        note1.intervalWith(givenNote) != 5 || note1 > givenNote,
        s"First note ($note1) must be a perfect consonance with the given note ($givenNote)")
      _ <- validateFourNoteMeasureRules
    } yield Pass
  }
}

case class AnacrusisMeasure(
    note2: Note,
    note3: Note,
    note4: Note,
    givenNote: Note)
    extends FirstMeasure
    with MultiNoteMeasure {
  override def notes: Vector[Note] = Vector(note2, note3, note4)

  override def validateMeasureRules: Evaluation = {
    for {
      _ <- rule(
        note2 consonantWith givenNote,
        s"Anacrusis first note ($note2) must be consonant with the given note ($givenNote)")
      _ <- validateMultiNoteMeasureRules
    } yield Pass
  }
}

sealed trait PenultimateMeasure extends Measure {
  lazy val lastNote: Note = nextMeasure.notes.head
  def noteBeforeLast: Note
  def twoBeforeLast: Note

  def validatePenultimateRules: Evaluation = {
    val lastNoteIsE = lastNote.pitchClass == 4
    lazy val wholeStepBeneathLast = Note(lastNote.id - 2)
    for {
      _ <- rule(
        !lastNoteIsE || (noteBeforeLast == Note(lastNote.octave, 2)),
        s"If the last note is an E, note before last ($noteBeforeLast) must be the D a step below")
      _ <- rule(
        lastNoteIsE || noteBeforeLast == Note(lastNote.id - 1),
        s"Second to last note ($noteBeforeLast) must be half step beneath final note ($lastNote)")
      _ <- rule(
        lastNoteIsE || !notes.contains(wholeStepBeneathLast),
        s"Penultimate measure cannot contain a $wholeStepBeneathLast when the last note is a $lastNote"
      )
      _ <- rule(
        lastNote.pitchClass != 9 || twoBeforeLast != Note(lastNote.octave, 5),
        s"The third to last note ($twoBeforeLast) must note be an F if the last note is an A ($lastNote)"
      )
    } yield Pass
  }
}

case class PenultimateNormalMeasure(
    note1: Note,
    note2: Note,
    note3: Note,
    note4: Note,
    givenNote: Note)
    extends PenultimateMeasure
    with FourNoteMeasure {

  override val twoBeforeLast: Note = note3
  override val noteBeforeLast: Note = note4

  override def validateMeasureRules: Evaluation = {
    for {
      _ <- rule(
        note1 != givenNote,
        s"The first note of a measure ($note1) cannot be the same as the given note ($givenNote)")
      _ <- Measure.validateFirstNote(this, prevMeasure)
      _ <- validateFourNoteMeasureRules
      _ <- validatePenultimateRules
    } yield Pass
  }
}

case class PenultimateSingletonMeasure(note: Note, givenNote: Note)
    extends PenultimateMeasure
    with WholeNoteMeasure {
  override val noteBeforeLast: Note = note
  override val twoBeforeLast: Note = prevMeasure.notes.last

  override def validateMeasureRules: Evaluation = {
    for {
      _ <- Measure.validateFirstNote(this, prevMeasure)
      _ <- validatePenultimateRules
    } yield Pass
  }
}

case class FinalMeasure(note: Note, givenNote: Note) extends WholeNoteMeasure {

  override def validateMeasureRules: Evaluation = {
    for {
      _ <- rule(
        Vector(1, 8).contains(note intervalWith givenNote),
        s"Last note ($note) must be equal to or an octave from the last given note ($givenNote)")
      _ <- rule(
        Measure.isContraryMotion(prevMeasure, this),
        s"If the first note ($note) is a 1 or 8 interval from the given note ($givenNote), contrary motion is required"
      )
    } yield Pass
  }
}
