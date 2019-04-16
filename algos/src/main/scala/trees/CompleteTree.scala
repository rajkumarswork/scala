package rajkumars.info.trees

import scala.collection.mutable.ArrayBuffer

// Complete Nary-Tree (left-justified with max-fanout = n and depth log_n(x)
class CompleteTree[T](n: Int) {

  private val ts = ArrayBuffer[T]()

  override def toString(): String =
    n + ": " + ts.zipWithIndex
      .map { case (v, i) => s"""$i,$v""" }
      .mkString(",\t")

  def size(): Int = ts.size
  def getOption(i: Int): Option[T] =
    if (ts.isDefinedAt(i)) Some(ts(i)) else None
  def get(i: Int): T = ts(i)
  def append(n: T): Unit = ts.append(n)
  def prepend(n: T): Unit = ts.prepend(n)
  def top(): Option[T] = ts.headOption
  def pop(): Option[T] = {
    val res = top()
    if (ts.isDefinedAt(0)) ts.remove(0)
    res
  }
  // parents and children
  def hasChild(i: Int, childNo: Int): Boolean = child(i, childNo).isDefined
  def child(i: Int, childNo: Int): Option[T] = {
    val ci = childIndex(i, childNo)
    if (ts.isDefinedAt(ci)) Some(ts(ci)) else None
  }
  def parent(i: Int): Option[T] = {
    val pi = parentIndex(i)
    if (pi >= 0 && pi < ts.size) Some(ts(pi)) else None
  }
  def flipWithParent(i: Int): Unit = flip(i, parentIndex(i))
  def flip(i: Int, j: Int): Unit = {
    if (i >= 0 && i < ts.size && j >= 0 && j < ts.size && i != j) {
      val tmp = ts(i)
      ts(i) = ts(j)
      ts(j) = tmp
    }
  }
  def isDefinedAt(i: Int): Boolean = ts.isDefinedAt(i)

  def log(k: Double): Double = CompleteTree.log(k, n)
  def parentIndex(i: Int): Int = CompleteTree.parentIndex(i, n)
  def childIndex(i: Int, childNo: Int): Int =
    CompleteTree.childIndex(i, childNo, n)
  def depth(i: Int): Int = CompleteTree.depth(i, n)
  def elementCount(d: Int): Int = CompleteTree.elementCount(d, n)
}

object CompleteTree {
  val BadIndex = -1
  def log(k: Double, n: Int): Double = Math.log(k) / Math.log(n)
  // parents and children
  def parentIndex(i: Int, n: Int): Int = (i - 1) / n
  // childNo is 0-indexed (leftmost child is 0, rightmost is n-1)
  def childIndex(i: Int, childNo: Int, n: Int): Int =
    n * i + 1 + childNo
  def depth(i: Int, n: Int): Int = log(i + 1.5, n).toInt
  def elementCount(d: Int, n: Int): Int =
    d match {
      case 0      => 1
      case r: Int => elementCount(r - 1, n) + Math.pow(n, r).toInt
    }
}
