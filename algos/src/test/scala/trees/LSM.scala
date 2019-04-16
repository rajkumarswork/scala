package rajkumars.info.trees

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

// Scalatest has serialization-close bug as of v3.1.0 Need to use junit
class LSMTests extends TestCase with Serializable {

  case class Edge(from: Int, to: Int) extends Serializable
  val d = "/temp/testing/"
  val aes = Seq[Edge](Edge(1, 2), Edge(2, 3), Edge(1, 2))
  val bes = Seq[Edge](Edge(2, 3), Edge(1, 2), Edge(3, 4))
  val ces = Seq[Edge](Edge(3, 2), Edge(4, 3), Edge(1, 3))
  val des = Seq[Edge](Edge(2, 3), Edge(3, 5), Edge(5, 4))
  val es = Seq(aes, bes, ces, des)

  override def setUp {}

  def testAddRemovee {
    val lsm = new LSMTree[Edge](d)
    lsm.add(aes.toIterator); lsm.flushToDisk()

    val de = Edge(2, 3)
    var result = lsm.exists(de)
    assertEquals("add should work", true, result)

    lsm.remove(de)
    result = lsm.exists(de)
    assertEquals("deletes should remove all copies", false, result)

    lsm.add(bes.toIterator)
    result = lsm.exists(de)
    assertEquals("adds after deletes should restore", true, result)
  }

  def testScan {
    val lsm = new LSMTree[Edge](d)
    lsm.add(aes.toIterator); lsm.flushToDisk()
    lsm.add(bes.toIterator); lsm.flushToDisk()
    val size = lsm.scan().size
    assertEquals("scans should retrieve all records", 5, size)
  }

  def testConsolidate {
    val lsm = new LSMTree[Edge](d)
    es.foreach(edges => { lsm.add(edges.toIterator); lsm.flushToDisk() })
    val startEpochs = lsm.epochs.size
    lsm.consolidate()
    val endEpochs = lsm.epochs.size
    val expected = (startEpochs + 1) / 2
    assertEquals("consolidation should reduce epochs by 2", expected, endEpochs)
  }

  def testMerge {
    val lsm = new LSMTree[Edge](d)
    lsm.add(aes.toIterator); lsm.flushToDisk()
    lsm.add(bes.toIterator); lsm.flushToDisk()
    lsm.merge(0, 1)
    val size = lsm.scan().size
    assertEquals("merges should eliminate duplicates", 3, size)
  }
}
