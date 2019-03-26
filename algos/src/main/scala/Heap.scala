class Heap[T](implicit cmp: Ordering[_ >: T]) {
  import cmp._

  val ts = new CompleteTree[T](2) // use a complete-binary tree
  override def toString(): String = ts.toString()
  val Left = 0
  val Right = 1

  def top(): Option[T] = ts.top()
  def push(n: T): Unit = {
    ts.append(n)
    floatUp()
  }
  def pop(): Option[T] = {
    val res = ts.pop()
    floatDown(0)
    res
  }

  // push the value at index i up until its parent in no longer smaller
  private def floatUp(i: Int = ts.size - 1): Unit = {
    val p = ts.parent(i)
    if (i > 0 && p.isDefined && p.get < ts.get(i)) {
      ts.flipWithParent(i)
      floatUp(ts.parentIndex(i))
    }
  }
  // push the value at index i down until its children are not greater
  private def floatDown(i: Int): Unit = {
    val li = largestIndex(i)
    if (li != i) {
      ts.flip(li, i)
      floatDown(li)
    }
  }
  // largest value/index among node and children
  private def largest(i: Int): T = {
    val nv = ts.get(i)
    val lc = if (ts.hasChild(i, Left)) ts.child(i, Left).get else nv
    val rc = if (ts.hasChild(i, Right)) ts.child(i, Right).get else nv
    max(max(nv, lc), rc)
  }
  private def largestIndex(i: Int): Int =
    largest(i) match {
      case nv if (nv == ts.get(i))              => i
      case lv if (lv == ts.child(i, Left).get)  => ts.childIndex(i, Left)
      case rv if (rv == ts.child(i, Right).get) => ts.childIndex(i, Right)
      case _                                    => i
    }
  private def max(a: T, b: T) = if (a >= b) a else b
}
