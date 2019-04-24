package rajkumars.info.net

import org.scalatest.FlatSpec

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class UDPUtilTest extends FlatSpec {

  "UDPUtil" should "send and recieve UDP packets correctly" in {
    val testString = "foo bar goo hoo"
    val s = UDPUtil.server(UDPUtil.Port, f)
    val c = UDPUtil.client(UDPUtil.Port)
    c.write(testString.getBytes)
    val result = new String(c.read())
    assert(result === testString)
  }

  def f(bs: Array[Byte]): Future[Array[Byte]] = Future { bs }

}
