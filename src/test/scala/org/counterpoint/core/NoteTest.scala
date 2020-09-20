package org.counterpoint.core

import org.counterpoint.core.Validator.Pass
import org.scalatest.flatspec.AsyncFlatSpec

class NoteTest extends AsyncFlatSpec {
  behavior of "Note"

  it should "compute intervals correctly" in {
    assert(Note.interval(C4, C4) == 1)
    assert(Note.interval(F4, B4) == 4)
    assert(Note.interval(C4, E5) == 3)
    assert(Note.interval(C4, C5) == 8)
    assert(Note.interval(C4, C4Sharp) == 1)
  }

  it should "compute consonances and dissonances correctly" in {
    assert(Note.isConsonant(C4, G4))
    assert(Note.isDissonant(C4, C4Sharp))
    assert(Note.isDissonant(C4, G4Sharp))
    assert(Note.isDissonant(C4, B4Flat))
    assert(Note.isDissonant(B2, F3))
    assert(Note.isConsonant(B2Flat, F3))
    assert(Note.isConsonant(F4, D4))
    assert(Note.isConsonant(E4, E5))
    assert(Note.isConsonant(F4, A5))
  }

  def correctWithModification(
      measure: Int,
      note: Int,
      replaceWith: Note): Vector[Measure] = {
    val measure1 = NormalFirstMeasure(D4, E4, F4, G4, D4)
    val measure2 = NormalMeasure(A4, B4, C5, D5, F4)
    val measure3 = NormalMeasure(E5, D5, B4, C5, E4)
    val measure4 = NormalMeasure(D5, C5, B4, A4, D4)
    val measure5 = NormalMeasure(B4, C5, D5, E5, G4)
    val measure6 = NormalMeasure(F5, G5, A5, G5, F4)
    val measure7 = NormalMeasure(F5, E5, C5, D5, A4)
    val measure8 = NormalMeasure(E5, F5, G5, G4, G4)
    val measure9 = NormalMeasure(A4, B4, C5, D5, F4)
    val measure10 = PenultimateNormalMeasure(E5, D5, B4, C5Sharp, E4)
    val measure11 = FinalMeasure(D5, D4)

    val correctMeasures = Vector(
      measure1,
      measure2,
      measure3,
      measure4,
      measure5,
      measure6,
      measure7,
      measure8,
      measure9,
      measure10,
      measure11
    )

    if (measure == 1) {
      val notes = measure1.notes.updated(note - 1, replaceWith)
      val newFirstMeasure = NormalFirstMeasure(notes(0),
                                               notes(1),
                                               notes(2),
                                               notes(3),
                                               measure1.givenNote)
      correctMeasures.updated(0, newFirstMeasure)
    } else if (measure == 10) {
      val notes = measure10.notes.updated(note - 1, replaceWith)
      val newTenthMeasure = PenultimateNormalMeasure(notes(0),
                                                     notes(1),
                                                     notes(2),
                                                     notes(3),
                                                     measure10.givenNote)
      correctMeasures.updated(9, newTenthMeasure)
    } else if (measure == 11) {
      val newLastMeasure = FinalMeasure(replaceWith, measure11.givenNote)
      correctMeasures.updated(10, newLastMeasure)
    } else {
      val measureToChange = correctMeasures(measure - 1)
      val changedMeasure = NormalMeasure(
        measureToChange.notes.updated(note - 1, replaceWith),
        measureToChange.givenNote)
      correctMeasures.updated(measure - 1, changedMeasure)
    }
  }

  it should "pass correct example" in {
    // Not a modification, that's what's there
    val measures = correctWithModification(1, 1, D4)

    assert(Validator.validate(measures) == Pass)
  }

  it should "fail on illegal dissonance" in {
    val measures = correctWithModification(4, 2, E5)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on first note consonance rule" in {
    val measures = correctWithModification(1, 1, B4)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on high note rule" in {
    val measures = correctWithModification(10, 1, C5)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on penultimate note too low" in {
    val measures = correctWithModification(10, 4, C5)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on consecutive notes" in {
    val measures = correctWithModification(5, 2, B4)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on large descending leaps" in {
    val measures = correctWithModification(8, 4, E4)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on large ascending leaps" in {
    val measures = correctWithModification(1, 2, D3)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on first note of measure illegal consonance" in {
    val measures = correctWithModification(9, 1, F4)

    assert(Validator.validate(measures) != Pass)
  }

  it should "fail on non-contrary motion when required" in {
    val measures = correctWithModification(5, 1, D5)

    assert(Validator.validate(measures) != Pass)
  }
}
