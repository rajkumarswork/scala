package rajkumars.info.util

import org.scalatest.FlatSpec

class LangTest extends FlatSpec {

  "Lang.hashCOde" should "calculate string hashes correclty" in {
    val s = "dd"
    val res = Lang.hashCode(s)
    val expected = s.hashCode
    assert(res === expected)
  }
  "Lang.hashCode" should "calculate rolling hashes correclty" in {
    val str = "dddd dddd"
    val sub = "dddd"
    val res = Lang.rollingHash(str, sub.size)
    val expected = str.sliding(sub.size).map( _.hashCode ).toSeq
    assert(res === expected)
  }
  "Lang.substring" should "pick substrings correctly" in {
    val s = "some long string that we want to search for sub strings"
    val n = "string"
    val o = "substring"
    val resn = Lang.substring(s,n)
    val reso = Lang.substring(s,o)
    assert(resn === 10 && reso == -1)
  }
}
