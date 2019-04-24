package rajkumars.info.net

import java.net._
import java.io._

import scala.util.{Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class UDPClient(s: DatagramSocket, sip: InetAddress, sport: Int) {
  val buffer = new Array[Byte](UDPUtil.BufferSize)
  def read(): Array[Byte] = {
    val inPacket = new DatagramPacket(buffer, buffer.size)
    s.receive(inPacket)
    buffer.take(inPacket.getLength)
  }
  def write(
      bytes: Array[Byte],
      address: InetAddress = sip,
      port: Int = sport
  ): Unit =
    s.send(new DatagramPacket(bytes, bytes.size, address, port))
}

object UDPUtil {

  val Port = 12345
  val IP = "69.202.251.129"
  val BufferSize = 16384

  def server(port: Int, f: (Array[Byte]) => Future[Array[Byte]]) = Future {
    println(s"Starting UDP Server on port: $Port")
    val buffer = new Array[Byte](BufferSize)
    val socket = new DatagramSocket(port)
    while (true) {
      val pk = new DatagramPacket(buffer, buffer.size)
      val res = socket.receive(pk)
      handleRequest(
        socket,
        f,
        buffer.take(pk.getLength),
        pk.getAddress,
        pk.getPort
      )
    }
  }

  private def handleRequest(
      s: DatagramSocket,
      f: (Array[Byte]) => Future[Array[Byte]],
      data: Array[Byte],
      address: InetAddress,
      port: Int
  ) =
    Future {
      f(data).onComplete {
        case Success(res) =>
          if (res.size > 0)
            s.send(new DatagramPacket(res, res.size, address, port))
      }
    }

  def client(port: Int, ip: String = "127.0.0.1"): UDPClient =
    new UDPClient(new DatagramSocket(), InetAddress.getByName(ip), port)

  // --------
  def f(bs: Array[Byte]): Future[Array[Byte]] = Future { bs }

  def runServer() = {
    val future = server(Port, f)
    while (true) Thread.sleep(60000)
  }

  def connectToServer(ip: String) = {
    val c = client(Port, ip)
    for (i <- (1 to 10)) {
      val bs = "foo foo".getBytes
      c.write(bs)
      println(new String(c.read()))
      Thread.sleep(5000)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.size > 0) connectToServer(args(0))
    else runServer()
  }
}
