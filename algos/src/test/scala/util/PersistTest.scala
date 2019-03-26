package util

import org.scalatest.FlatSpec

class PersistTest extends FlatSpec {

  case class Foo(i: Int, j: String)
  val a = Foo(1, "bar")
  val tfname = "/temp/ftest.bin"
  val jfname = "/temp/jtest.json"

  "Persist" should "save and restore objects correctly" in {
    Persist.toFile(a, tfname)
    val res = Persist.fromFile[Foo](tfname)
    assert(res.get === a)
  }
  "Persist" should "save and restore json objects correctly" in {
    Persist.toFileJson(a, jfname)
    val res = Persist.fromFileJson[Foo](jfname)
    assert(res.get === a)
  }
}
