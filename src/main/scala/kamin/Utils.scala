package kamin

import scala.collection.mutable

class PeekingIterator[A](iter: Iterator[A]) extends Iterator[A]:
  private var buffer: List[A] = List.empty

  // Peek at the next `n` elements without consuming them
  def peek(n: Int): List[A] =
    if n <= buffer.size then
      buffer.take(n)
    else 
      val elementsToFetch = n - buffer.size
      val newElements = iter.take(elementsToFetch).toList
      buffer = buffer ++ newElements
      buffer.take(n)

  // Has next only if there are elements in the buffer or the original iterator
  override def hasNext: Boolean = buffer.nonEmpty || iter.hasNext

  // Return the next element, consuming the buffer if necessary
  override def next(): A = 
    if buffer.nonEmpty then
      val head = buffer.head
      buffer = buffer.tail
      head
    else 
      iter.next()

class MultiKeyContainer[A, Value]:
  private val container = mutable.Map[Seq[A], Value]()

  // Insert a value with a tuple key (can be of varying length)
  def put(key: A*)(value: Value): Unit =
    container.put(key.toSeq, value)

  // Lookup a value with a tuple key (can be of varying length)
  def get(key: A*): Option[Value] =
    container.get(key.toSeq)

  // Get all values associated with keys that start with the given prefix
  def getAllWithPrefix(prefix: A*): Seq[Value] =
    container.collect {
      case (k, v) if k.startsWith(prefix.toSeq) => v
    }.toSeq
