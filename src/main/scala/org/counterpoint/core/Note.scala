package org.counterpoint.core

sealed trait Note {
  def pitchClass: Int
  def octave: Int

  def letterNumber: Int

  def id: Int = 12 * octave + pitchClass
}

object Note {

  def interval(note1: Note, note2: Note): Int = {
    val (highNote, lowNote) = if (note1.id > note2.id) {
      (note1, note2)
    } else {
      (note2, note1)
    }

    val letterNumDiff =
      Math.abs(highNote.letterNumber - lowNote.letterNumber) + 1

    val intervalDiff = if (highNote.letterNumber >= lowNote.letterNumber) {
      letterNumDiff
    } else {
      9 - letterNumDiff
    }

    require(
      pitchDiff(note1, note2) <= 16,
      s"Notes given are too far apart for interval comparison: $note1, $note2, pitchDiff=${pitchDiff(note1, note2)}")

    if (note1.pitchClass == note2.pitchClass && note1 != note2) {
      8
    } else {
      intervalDiff
    }
  }

  def pitchDiff(note1: Note, note2: Note): Int = {
    Math.abs(note1.id - note2.id)
  }

  def isDissonant(note1: Note, note2: Note): Boolean = {
    val diff = pitchDiff(note1, note2)
    interval(note1, note2) match {
      case 2 | 4 | 7                   => true
      case 5 if diff != 7              => true
      case 1 | 8 if diff != 0          => true
      case 3 if diff != 3 || diff != 4 => true
      case 6 if diff != 8 || diff != 9 => true
      case _                           => false
    }
  }

  def isConsonant(note1: Note, note2: Note): Boolean = {
    !isDissonant(note1, note2)
  }

  def isTritone(note1: Note, note2: Note): Boolean = {
    pitchDiff(note1, note2) == 6
  }
}

sealed trait C extends Note {
  override def pitchClass: Int = 0

  override def letterNumber: Int = 0
}

sealed trait CSharp extends Note {
  override def pitchClass: Int = 1

  override def letterNumber: Int = 0
}

sealed trait D extends Note {
  override def pitchClass: Int = 2

  override def letterNumber: Int = 1
}

sealed trait EFlat extends Note {
  override def pitchClass: Int = 3

  override def letterNumber: Int = 2
}

sealed trait E extends Note {
  override def pitchClass: Int = 4

  override def letterNumber: Int = 2
}

sealed trait F extends Note {
  override def pitchClass: Int = 5

  override def letterNumber: Int = 3
}

sealed trait FSharp extends Note {
  override def pitchClass: Int = 6

  override def letterNumber: Int = 3
}

sealed trait G extends Note {
  override def pitchClass: Int = 7

  override def letterNumber: Int = 4
}

sealed trait GSharp extends Note {
  override def pitchClass: Int = 8

  override def letterNumber: Int = 4
}

sealed trait A extends Note {
  override def pitchClass: Int = 9

  override def letterNumber: Int = 5
}

sealed trait BFlat extends Note {
  override def pitchClass: Int = 10

  override def letterNumber: Int = 6
}

sealed trait B extends Note {
  override def pitchClass: Int = 11

  override def letterNumber: Int = 6
}

sealed trait Octave2Note extends Note {
  override def octave: Int = 2
}

sealed trait Octave3Note extends Note {
  override def octave: Int = 3
}

sealed trait Octave4Note extends Note {
  override def octave: Int = 4
}

sealed trait Octave5Note extends Note {
  override def octave: Int = 5
}

sealed trait Octave6Note extends Note {
  override def octave: Int = 6
}

case object C2 extends C with Octave2Note
case object C2Sharp extends CSharp with Octave2Note
case object D2 extends D with Octave2Note
case object E2Flat extends EFlat with Octave2Note
case object E2 extends E with Octave2Note
case object F2 extends F with Octave2Note
case object F2Sharp extends FSharp with Octave2Note
case object G2 extends G with Octave2Note
case object G2Sharp extends GSharp with Octave2Note
case object A2 extends A with Octave2Note
case object B2Flat extends BFlat with Octave2Note
case object B2 extends B with Octave2Note

case object C3 extends C with Octave3Note
case object C3Sharp extends CSharp with Octave3Note
case object D3 extends D with Octave3Note
case object E3Flat extends EFlat with Octave3Note
case object E3 extends E with Octave3Note
case object F3 extends F with Octave3Note
case object F3Sharp extends FSharp with Octave3Note
case object G3 extends G with Octave3Note
case object G3Sharp extends GSharp with Octave3Note
case object A3 extends A with Octave3Note
case object B3Flat extends BFlat with Octave3Note
case object B3 extends B with Octave3Note

case object C4 extends C with Octave4Note
case object C4Sharp extends CSharp with Octave4Note
case object D4 extends D with Octave4Note
case object E4Flat extends EFlat with Octave4Note
case object E4 extends E with Octave4Note
case object F4 extends F with Octave4Note
case object F4Sharp extends FSharp with Octave4Note
case object G4 extends G with Octave4Note
case object G4Sharp extends GSharp with Octave4Note
case object A4 extends A with Octave4Note
case object B4Flat extends BFlat with Octave4Note
case object B4 extends B with Octave4Note

case object C5 extends C with Octave5Note
case object C5Sharp extends CSharp with Octave5Note
case object D5 extends D with Octave5Note
case object E5Flat extends EFlat with Octave5Note
case object E5 extends E with Octave5Note
case object F5 extends F with Octave5Note
case object F5Sharp extends FSharp with Octave5Note
case object G5 extends G with Octave5Note
case object G5Sharp extends GSharp with Octave5Note
case object A5 extends A with Octave5Note
case object B5Flat extends BFlat with Octave5Note
case object B5 extends B with Octave5Note

case object C6 extends C with Octave6Note
case object C6Sharp extends CSharp with Octave6Note
case object D6 extends D with Octave6Note
case object E6Flat extends EFlat with Octave6Note
case object E6 extends E with Octave6Note
case object F6 extends F with Octave6Note
case object F6Sharp extends FSharp with Octave6Note
case object G6 extends G with Octave6Note
case object G6Sharp extends GSharp with Octave6Note
case object A6 extends A with Octave6Note
case object B6Flat extends BFlat with Octave6Note
case object B6 extends B with Octave6Note
