package rajkumars.info.trees

import scala.collection.mutable.{Map, SortedMap}

class BPlusTree[K, R](b: Int)(implicit o: Ordering[K]) {
  import o._

  // api
  def add(k: K, r: R) = {
    val res = root.add(k, r)
    if (res.size == 2)
      root = new Node(Some(res.head), SortedMap(res.last.key() -> res.last))
  }
  def remove(k: K) = {
    root.remove(k)
    if (root.size == 0) root = new Leaf(SortedMap[K, R](), None) // Fix
  }
  def get(k: K): Option[R] = root.get(k)
  def size(): Int = root.size()

  // --
  private val Debug = false
  override def toString(): String = s"""\n<${root.toString}>\n"""
  private var root: Node = new Leaf(SortedMap[K, R](), None)

  // ----------
  class Node(
      var left: Option[Node] = None,
      nodem: Map[K, Node] = SortedMap[K, Node]()
  ) {
    def key(): K =
      if (left.isEmpty || left.get.size() == 0) nodem.head._1
      else left.get.key()
    def size(): Int =
      nodem.size + (if (left.isEmpty || left.get.size == 0) 0 else 1)
    def add(k: K, r: R): Seq[Node] = {
      val tree = getNode(k)
      val res = tree.add(k, r)
      if (res.size == 2) {
        val l = res.head
        val r = res.last
        nodem(r.key()) = r
        if (tree == left.get)
          left = Some(r)
        else
          nodem(l.key()) = l
      }
      if (size() > b) split()
      else Seq[Node](this)
    }
    def get(k: K): Option[R] = getNode(k).get(k)
    def remove(k: K): Unit = {
      val tree = getNode(k)
      tree.remove(k)
      nodem.remove(k)
      if (tree.size() > 0) nodem(tree.key()) = tree
      if (nodem.size == 0) rebalance()
      if (Debug) println(s"""removed $k => $tree\n${this}""")
    }
    def empty(): Unit = { left = None; nodem.clear }
    def split(): Seq[Node] = {
      val rs = nodem.drop(b / 2)
      val r = new Node(Some(rs.head._2), rs.tail)
      val l = new Node(left, nodem.take(b / 2))
      if (Debug) println(s"""split into ($l and $r)""")
      Seq(l, r)
    }
    def rebalance(): Unit =
      if (nodem.size == 0 && left.get.size() > 0) {
        nodem(left.get.key()) = left.get
        left.get.empty()
      }
    // get the tree that potentially has the key k
    private def getNode(k: K): Node = {
      val res =
        if (k < nodem.head._1) left.get
        else if (k >= nodem.last._1) nodem.last._2
        else nodem.takeWhile(x => x._1 <= k).last._2
      if (Debug) println(s"$k => $res")
      res
    }
    override def toString(): String =
      s"{(${key()}) " + left.get.toString + " : " +
        nodem
          .map { case (k, n) => s"""($k):${n.toString}""" }
          .mkString("\n") + "}\n"
  }

  // - -
  class Leaf(data: Map[K, R], var next: Option[Leaf]) extends Node {
    override def get(k: K): Option[R] = data.get(k)
    override def key(): K = data.head._1
    override def size(): Int = data.size
    override def empty(): Unit = { data.clear; next = None }
    override def rebalance(): Unit = {}
    override def remove(k: K): Unit = data.remove(k)
    override def add(k: K, r: R): Seq[Leaf] = {
      data(k) = r
      if (size() > b) split()
      else Seq[Leaf](this)
    }
    override def split(): Seq[Leaf] = {
      val r = new Leaf(data.drop(b / 2), None)
      Seq(new Leaf(data.take(b / 2), Some(r)), r)
    }
    override def toString(): String = {
      val n = if (next.isDefined) "o" else "e"
      "[" + data.map { case (k, v) => s"$k=$v" }.mkString(", ") + "->" + n + "]"
    }
  }
}
