package rajkumars.info.net

import org.scalatest.FlatSpec

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class TCPUtilTest extends FlatSpec {

  "TCPUtil" should "send and recieve UDP packets correctly" in {
    val testString = "foo bar"
    val s = TCPUtil.server(TCPUtil.Port, f)
    val c = TCPUtil.client(TCPUtil.Port)
    c.write(testString.getBytes)
    val result = new String(c.read())
    assert(result === testString)
  }

  def f(bs: Array[Byte]): Future[Array[Byte]] = Future { bs }
}
