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

  it should "pass correct example" in {
    val measures = Vector(
      NormalFirstMeasure(D4, E4, F4, G4, D4),
      NormalMeasure(A4, B4, C5, D5, F4),
      NormalMeasure(E5, D5, B4, C5, E4),
      NormalMeasure(D5, C5, B4, A4, D4),
      NormalMeasure(B4, C5, D5, E5, G4),
      NormalMeasure(F5, G5, A5, G5, F4),
      NormalMeasure(F5, E5, C5, D5, A4),
      NormalMeasure(E5, F5, G5, G4, G4),
      NormalMeasure(A4, B4, C5, D5, F4),
      PenultimateNormalMeasure(E5, D5, B4, C5Sharp, E4),
      FinalMeasure(D5, D4)
    )

    assert(Validator.validate(measures) == Pass)
  }
}
