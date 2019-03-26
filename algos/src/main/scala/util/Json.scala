package util

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import java.io._
import scala.util.Try

// Note: Map keys need to be strings
object Json {
  implicit val formats = Serialization.formats(NoTypeHints)

  // Json Serialization
  def dump[A <: AnyRef](o: A)(
      implicit m: ClassManifest[A]): String = write(o)

  def load[A: Manifest](s: String)(
      implicit m: ClassManifest[A]): Option[A] = Try(read[A](s)).toOption
}
