package rajkumars.info.util

import org.scalatest.FlatSpec

class IteratorsTest extends FlatSpec {

  "Iterators.merge" should "merge a list of iterators correctly" in {
    val itList = List(
      Iterator[Int](1, 4, 7),
      Iterator[Int](0, 1, 5, 9),
      Iterator[Int](1, 2, 3, 6, 8)
    )
    val newIt = Iterators.merge[Int](itList)
    val expected = Iterator[Int](0, 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(newIt.toSeq === expected.toSeq)
  }

  "Iterators.mergeDistinct" should "merge a list of iterators correctly" in {
    val itList = List(
      Iterator[Int](1, 4, 7),
      Iterator[Int](0, 1, 5, 9),
      Iterator[Int](1, 2, 3, 6, 8)
    )
    val newIt = Iterators.mergeDistinct[Int](itList)
    val expected = Iterator[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(newIt.toSeq === expected.toSeq)
  }

  "Iterators.distinct" should "only produce distinct elements" in {
    case class Foo(i: Int, j: String) extends Ordered[Foo] {
      override def compare(that: Foo) = i compare that.i
    }
    object SOrd extends Ordering[Foo] {
      def compare(a: Foo, b: Foo) = b.j.size compare a.j.size
    }
    val a = Iterator(Foo(1, "one"), Foo(1, "1"), Foo(2, "two"), Foo(2, "2"))
    val dit = Iterators.distinct[Foo](a)(SOrd)
    val expected = Iterator(Foo(1, "one"), Foo(2, "two"))
    assert(dit.toSeq === expected.toSeq)
  }
}
