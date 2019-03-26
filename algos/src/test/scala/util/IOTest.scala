package util

import org.scalatest.FlatSpec

class IOTest extends FlatSpec {

  case class Foo(i: Int, j: String) extends Serializable
  val a = Foo(1, "bar")
  val fname = "/temp/test.out"

  "IO" should "save and restore objects correctly" in {
    val ba = IO.dump(a)
    val res = IO.load[Foo](ba)
    assert(res.get === a)
  }
  "IO" should "save and restore to file" in {
    val ba = IO.dump(a)
    IO.toFile(ba, fname)
    val rba = IO.fromFile(fname)
    val res = IO.load[Foo](rba)
    assert(res.get === a)
  }
}
