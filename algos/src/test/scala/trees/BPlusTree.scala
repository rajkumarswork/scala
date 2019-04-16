package rajkumars.info.trees

import org.scalatest.FlatSpec

import scala.collection.mutable.Map

class BPlusTreeTest extends FlatSpec {

  val t = new BPlusTree[Int, Int](3)

  "Adding and removing same elements" should "result in empty tree" in {
    t.add(1, 1); t.add(2, 2); t.add(3, 3); t.add(4, 4)
    t.remove(1); t.remove(2); t.remove(3); t.remove(4)
    assert(t.size() === 0)
  }
}
