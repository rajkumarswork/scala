package rajkumars.info.util

import java.io._
import java.util.zip._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.IOUtils

// Tools to zip/unzip data (can be gzip compatible)
object Zip {

  val Level = 5 // can be 0-9

  def compress(str: String): Array[Byte] = {
    val bytes = str.getBytes("UTF-8")
    val compressor = new Deflater(Level, true)
    compressor.setInput(bytes)
    compressor.finish
    val output = new Array[Byte](str.size * 2)
    val l = compressor.deflate(output)
    output.take(l)
  }

  // gzip compatible inflation (norap Inflator) requires a dummy byte at end
  def decompress(ba: Array[Byte]): String = {
    val xba = ba ++ Array[Byte](0)
    val decompressor = new Inflater(true)
    decompressor.setInput(xba, 0, xba.size)
    val res = new Array[Byte](1000)
    val l = decompressor.inflate(res)
    decompressor.end
    new String(res, 0, l, "UTF-8")
  }

  // - gz codecs --
  def gzCompress(s: String): Array[Byte] = {
    val o = new java.io.ByteArrayOutputStream()
    val gos = new GZIPOutputStream(o)
    gos.write(s.getBytes("UTF-8")); gos.close
    o.toByteArray;
  }
  def gzUncompress(bs: Array[Byte]): String = {
    val gis = new GZIPInputStream(new java.io.ByteArrayInputStream(bs))
    scala.io.Source.fromInputStream(gis).getLines.mkString("")
  }
  def isGz(bs: Array[Byte]): Boolean =
    bs(0) == GZIPInputStream.GZIP_MAGIC.toByte && // 0x1f
      bs(1) == (GZIPInputStream.GZIP_MAGIC >> 8).toByte // 0x8b

}
