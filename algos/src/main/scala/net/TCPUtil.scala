package rajkumars.info.net

import java.net._
import java.io._
import java.util.concurrent.Executors

import scala.util.{Success, Failure}
import scala.concurrent.{Future, ExecutionContext}

class Connection(is: InputStream, os: OutputStream) {
  val buffer = new Array[Byte](TCPUtil.BufferSize)
  def read(): Array[Byte] = buffer.take(is.read(buffer))
  def write(bytes: Array[Byte]): Unit = os.write(bytes)
}

object TCPUtil {
  implicit val ec =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))

  val Port = 10920
  val IP = "69.202.251.129"
  val BufferSize = 16384

  def server(port: Int, f: (Array[Byte]) => Future[Array[Byte]]) = Future {
    println(s"Starting Server on port: $Port")
    val socket = new ServerSocket(port)
    while (true) handleConnection(socket.accept(), f)
  }

  private def handleConnection(
      s: Socket,
      f: (Array[Byte]) => Future[Array[Byte]]
  ) =
    Future {
      println(s"Accpeted connection from ${s.getInetAddress}:${s.getPort}")
      val buffer = new Array[Byte](BufferSize)
      val is = s.getInputStream()
      val os = s.getOutputStream()
      var bytesRead = 0
      while (bytesRead >= 0) {
        bytesRead = is.read(buffer)
        f(buffer.take(bytesRead)).onComplete {
          case Success(res) => if (res.size > 0) os.write(res)
        }
      }
    }

  def client(port: Int, ip: String = "127.0.0.1"): Connection = {
    val socket = new Socket(ip, port)
    new Connection(socket.getInputStream, socket.getOutputStream)
  }

  // --------
  def f(bs: Array[Byte]): Future[Array[Byte]] = Future { bs }

  def runServer() = {
    val future = server(Port, f)
    while (true) Thread.sleep(60000)
  }

  def connectToServer(ip: String) = {
    val c = client(Port, ip)
    for (i <- (1 to 1000)) {
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
