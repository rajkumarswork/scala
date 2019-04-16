package rajkumars.info.trees

import org.scalatest.FlatSpec

class CompleteTreeTest extends FlatSpec {

  val h = new CompleteTree[Int](2)
  h.append(3)
  h.append(20)
  h.append(12)

  "CompleteTree(2)" should "store the first-inserted on top" in {
    val expected = 3
    val result = h.top().get
    assert(expected === result)
  }
  it should "calculate parent correctly" in {
    val expected = 3
    val result = h.parent(1).get
    assert(expected === result)
  }
  it should "calculate parent-index correctly" in {
    val expected = 0
    val result = h.parentIndex(2)
    assert(expected === result)
  }
  it should "calculate children correctly" in {
    val expected = 20
    val result = h.child(0, 0).get
    assert(expected === result)
  }
  it should "calculate right children correctly" in {
    val expected = 12
    val result = h.child(0, 1).get
    assert(expected === result)
  }
  it should "calculate right children index correctly" in {
    val expected = 2
    val result = h.childIndex(0, 1)
    assert(expected === result)
  }
  it should "pop correctly" in {
    val expected = 20
    h.pop()
    val result = h.top().get
    assert(expected === result)
  }
}
