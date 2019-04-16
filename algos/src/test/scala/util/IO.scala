package rajkumars.info.util

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class IOTest extends TestCase with Serializable {

  case class Foo(i: Int, j: String) extends Serializable

  def testSave {
    val f = "/temp/testing/io.out"
    val fs = Seq(Foo(1, "bar"), Foo(2, "boo"))
    IO.toFile[Foo](fs.toIterator, f)
    val size = IO.fromFile[Foo](f).size
    assertEquals(
      "save and retrieve to FS should restore original",
      fs.size,
      size
    )
  }
}
