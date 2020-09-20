package org.counterpoint.core

sealed trait Mode {
  def contains(note: Note): Boolean
}

case object Ionian extends Mode {

  override def contains(note: Note): Boolean = {
    note match {
      case _: CSharp | _: GSharp => false
      case _                     => true
    }
  }
}

case object Dorian extends Mode {

  override def contains(note: Note): Boolean = {
    note match {
      case _: GSharp => false
      case _         => true
    }
  }
}

case object Phrygian extends Mode {

  override def contains(note: Note): Boolean = {
    note match {
      case _: CSharp | _: GSharp => false
      case _                     => true
    }
  }
}

case object Mixolydian extends Mode {

  override def contains(note: Note): Boolean = {
    note match {
      case _: CSharp | _: GSharp => false
      case _                     => true
    }
  }
}

case object Aeolian extends Mode {

  override def contains(note: Note): Boolean = {
    note match {
      case _: CSharp => false
      case _         => true
    }
  }
}
