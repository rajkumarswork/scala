package rajkumars.info.util

import java.io._
import java.nio.file.{Files, Paths, StandardCopyOption}
import scala.reflect.ClassManifest

object IO {

  // - File IO ---
  // Bytes
  def bytesToFile(ba: Array[Byte], fname: String) =
    Files.write(new File(fname).toPath(), ba)
  def bytesFromFile(fname: String): Array[Byte] =
    Files.readAllBytes(new File(fname).toPath())
  // Seek and read
  def bytesFromFile(
      fname: String,
      len: Int,
      startByte: Int = 0
  ): Array[Byte] = {
    val ba = new Array[Byte](len)
    val fis = new FileInputStream(fname)
    fis.getChannel.position(startByte)
    fis.read(ba, 0, len)
    ba
  }

  // Objects
  def size[A](a: A): Int = {
    val ba = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(ba)
    out.writeObject(a)
    out.close
    ba.size
  }
  def toFile[A](oit: Iterator[A], fname: String) = {
    val fos = new FileOutputStream(fname)
    val oos = new ObjectOutputStream(fos)
    oit.foreach(o => oos.writeObject(o))
    oos.close
  }

  // Note: Cannot check class because of java type-erasure (see dump and load)
  def fromFile[A](fname: String): Iterator[A] = {
    val fis = new FileInputStream(fname)
    val ois = new ObjectInputStream(fis)
    val it = Iterator
      .continually(if (fis.available > 0) Some(ois.readObject) else None)
      .takeWhile(_.isDefined)
      .map(_.get)
    // ois.close  (can not close until iterator is read)
    it.collect { case r: A => r }
  }

  // scala-native serialization (only works for classes, collections run into trouble)
  def dump[A](o: A)(implicit m: ClassManifest[A]): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(m)
    out.writeObject(o)
    out.close()
    ba.toByteArray
  }
  def load[A](buf: Array[Byte])(implicit m: ClassManifest[A]): Option[A] = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buf))
    val found = in.readObject.asInstanceOf[ClassManifest[_]]
    val res =
      if (found <:< m) Some(in.readObject.asInstanceOf[A])
      else None
    in.close()
    res
  }

  // File/Dir helpers
  def fileExists(path: String): Boolean = new java.io.File(path).isFile
  def dirExists(path: String): Boolean = new java.io.File(path).isDirectory
  def fileRename(old: String, newname: String): Boolean =
    Files.move(
      Paths.get(old),
      Paths.get(newname),
      StandardCopyOption.REPLACE_EXISTING
    ) != null

  // ----------------------
  def main(args: Array[String]): Unit = {
    val f = "/temp/testing/test.out"
    case class Foo(i: Int, j: String)
    val a = Iterator(Foo(1, "bar"), Foo(2, "two"))
    IO.toFile(a, f)
    val r = IO.fromFile[Foo](f)
    r.foreach(println)
  }
}
