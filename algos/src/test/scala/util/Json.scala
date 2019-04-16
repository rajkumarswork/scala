package rajkumars.info.util

import org.scalatest.FlatSpec

class JsonTest extends FlatSpec {

  case class Foo(i: Int, j: String)
  val fa = Foo(1, "bar")

  "Json" should "save and restore objects correctly" in {
    val ba = Json.dump(fa)
    val res = Json.load[Foo](ba).get
    assert(res.i === fa.i)
    assert(res.j === fa.j)
  }
}
