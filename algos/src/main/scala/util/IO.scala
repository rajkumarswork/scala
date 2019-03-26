package util

import java.io._
import java.nio.file.Files
import scala.reflect.ClassManifest

object IO {

  // File IO
  def toFile(ba: Array[Byte], fname: String) =
    Files.write(new File(fname).toPath(), ba)
  def fromFile(fname: String): Array[Byte] =
    Files.readAllBytes(new File(fname).toPath())

  // scala-native serialization (only works for classes, collections run into trouble)
  def dump[A](o: A)(implicit m: ClassManifest[A]): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(m)
    out.writeObject(o)
    out.close()
    ba.toByteArray
  }
  def load[A](buf: Array[Byte])(
      implicit m: ClassManifest[A]): Option[A] = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buf))
    val found = in.readObject.asInstanceOf[ClassManifest[_]]
    val res =
      if (found <:< m) Some(in.readObject.asInstanceOf[A])
      else None
    in.close()
    res
  }
}
