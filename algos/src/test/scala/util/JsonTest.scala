package util

import org.scalatest.FlatSpec

class JsonTest extends FlatSpec {

  case class Foo(i: Int, j: String)
  val a = Foo(1, "bar")
  val fname = "/temp/test.json"

  "Jsont" should "save and restore objects correctly" in {
    val ba = Json.dump(a)
    val res = Json.load[Foo](ba)
    assert(res.get == a)
  }
}
