package rajkumars.info.util

import org.scalatest.FlatSpec

class ZipTest extends FlatSpec {

  "Zip" should "zip and unzip correctly" in {
    val testString = "foo bar goo hoo"
    val result = Zip.decompress(Zip.compress(testString))
    assert(result === testString)
  }
}
