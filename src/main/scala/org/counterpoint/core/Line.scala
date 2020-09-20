package org.counterpoint.core

// TODO: Implement validation for line rules
case class Line(notes: Vector[Note]) extends IndexedSeq[Note] {
  override def iterator: Iterator[Note] = notes.iterator
  override def length: Int = notes.length
  override def apply(idx: Int): Note = notes(idx)
}
