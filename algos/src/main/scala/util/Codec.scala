package rajkumars.info.util

import javax.crypto._
import javax.crypto.spec._
import javax.xml.bind.DatatypeConverter

import java.util.Base64
// import org.apache.commons.codec.binary.Base64

// Routines for encryption/decryption

// Note: Ciphers are not threadsafe. Create one for each thread
object Codec {

  val eciphers = new ThreadLocal[Cipher]() {
    override def initialValue() = createCipher(true)
  }
  val dciphers = new ThreadLocal[Cipher]() {
    override def initialValue() = createCipher(false)
  }

  // - Api --------
  // Decrypt sometimes gets errors about unpadded encryptions
  def decrypt(encrypted: String): String =
    scala.util
      .Try(
        new String(
          dciphers.get().doFinal(DatatypeConverter.parseBase64Binary(encrypted))
        )
      )
      .getOrElse("decrypt_error")

  def encrypt(raw: String): String =
    DatatypeConverter.printBase64Binary(eciphers.get().doFinal(raw.getBytes()))

  // General purpose encoders/decoders
  // val b64 = new Base64
  def encode(ba: Array[Byte]): Array[Byte] = Base64.getEncoder.encode(ba)
  def decode(ba: Array[Byte]): Array[Byte] = Base64.getDecoder.decode(ba)
  def decode(str: String): Array[Byte] = Base64.getDecoder.decode(str)
  def urldecode(v: String) = java.net.URLDecoder.decode(v, "UTF-8")
  def urlencode(v: String) = java.net.URLEncoder.encode(v, "UTF-8")

  // - Private -------------------------------
  // Parameters
  val AES_KEY: Array[Byte] =
    Array(243, 53, 89, 109, 12, 48, 249, 235, 200, 47, 17, 2, 82, 22, 142, 87,
      29, 139, 194, 124, 43, 209, 142, 132, 240, 166, 226, 1, 157, 109, 53, 141)
      .map(_.toByte)
  val AES_IV: Array[Byte] =
    Array(192, 65, 29, 85, 193, 29, 62, 25, 69, 95, 16, 221, 149, 29, 128, 223)
      .map(_.toByte)

  val keyspec = new SecretKeySpec(AES_KEY, "AES")
  val paramspec = new IvParameterSpec(AES_IV)

  // cipher creator
  private def createCipher(encrypt: Boolean = true): Cipher = {
    val c = Cipher.getInstance("AES/CBC/PKCS5Padding")
    if (encrypt) c.init(Cipher.ENCRYPT_MODE, keyspec, paramspec)
    else c.init(Cipher.DECRYPT_MODE, keyspec, paramspec)
    c
  }
}
