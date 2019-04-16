package rajkumars.info.trees

object BSTree {
  case class BST(
      var v: Int = Int.MinValue,
      var left: Option[BST] = None,
      var right: Option[BST] = None
  ) {
    override def toString(): String = {
      val ls = if (left.isDefined) s"""\nl($v):${left.get.toString}""" else ""
      val rs = if (right.isDefined) s"""\nr($v):${right.get.toString}""" else ""
      s"""$v $ls $rs"""
    }
  }

  val EmptyBST = BST()
  def contains(v: Int, t: BST): Boolean = {
    v == t.v ||
    contains(v, t.left.getOrElse(EmptyBST)) ||
    contains(v, t.right.getOrElse(EmptyBST))
  }

  def insert(iv: Int, t: BST): BST =
    t match {
      case BST(v, l, r) if (iv <= v) =>
        if (l.isDefined) insert(iv, l.get)
        else {
          t.left = Some(BST(iv)); t
        }
      case BST(v, l, r) if (iv > v) =>
        if (r.isDefined) insert(iv, r.get)
        else {
          t.right = Some(BST(iv)); t
        }
      case _ => BST(iv)
    }

  def delete(iv: Int, t: BST): BST = t
  def balance(iv: Int, t: BST): BST = t

  def test() = {
    val tree = insert(2, null)
    insert(17, tree)
    insert(12, tree)
    println(tree)
  }
  def main(args: Array[String]): Unit = test()
}
