package org.counterpoint.core

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
  }
}
