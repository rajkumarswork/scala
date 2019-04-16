package rajkumars.info.util

import java.io._
import java.nio.file.Files
import scala.reflect.Manifest
import scala.reflect.ClassManifest
import scala.util.Try

object Persist {

  // api
  def toFile[A](o: A, f: String)(implicit m: ClassManifest[A]): Unit =
    IO.bytesToFile(IO.dump[A](o)(m), f)

  def fromFile[A](f: String)(implicit m: ClassManifest[A]): Option[A] =
    IO.load[A](IO.bytesFromFile(f))(m)

  def toFileJson[A <: AnyRef: Manifest](o: A, f: String): Unit =
    IO.bytesToFile(Json.dump[A](o).getBytes, f)

  def fromFileJson[A: Manifest](f: String): Option[A] =
    Json.load[A](new String(IO.bytesFromFile(f)))

  def main(args: Array[String]): Unit = {
    case class R(a: String, b: Int) extends Ordered[R] {
      def compare(that: R): Int = this.b.compare(that.b)
    }
    val f = "/temp/m.out"
    val a: Seq[R] = Seq[R](R("1", 1), R("2", 2)).sortBy(x => x)
    val s = toFileJson[Seq[R]](a, f)
    val r = fromFileJson[Seq[R]](f)
    print(r)
  }
}
