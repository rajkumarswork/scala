package rajkumars.info.trees

import org.scalatest.FlatSpec

class HeapTests extends FlatSpec {

  val h = new Heap[Int]()
  h.push(3)
  h.push(20)
  h.push(12)

  "Heap" should "store the max-value on top" in {
    val expected = 20
    val result = h.top().get
    assert(expected === result)
  }
  "Heap" should "pop correctly" in {
    val expected = 12
    h.pop
    val result = h.top().get
    assert(expected === result)
  }
}
