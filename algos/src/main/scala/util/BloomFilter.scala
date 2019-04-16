package rajkumars.info.util

import collection.mutable.BitSet
import scala.util._
import java.io._

// s: size of input (#entries)
// w: width(bits) of filter
// k: number of hashes/keys
// bs: the bitset to write to
class BloomFilter[A] private (
    val s: Int,
    val w: Int,
    val k: Int,
    var bs: BitSet
) extends Serializable {
  val r = scala.util.Random
  def this(entries: Iterator[A], size: Int) = {
    this(
      size,
      BloomFilter.bestW(size),
      BloomFilter.optimalK(size, BloomFilter.bestW(size)),
      BitSet()
    )
    add(entries)
    println(info)
  }
  def this(size: Int) = {
    this(
      size,
      BloomFilter.bestW(size),
      BloomFilter.optimalK(size, BloomFilter.bestW(size)),
      BitSet()
    )
  }
  //-API------
  def +(e: A) = add(e)
  def ++(es: Seq[A]) = for (e <- es) add(e)
  def apply(e: A): Boolean = contains(e)

  def contains(e: A): Boolean = {
    r.setSeed(e.hashCode)
    val is = Stream.continually(bs(r.nextInt(w))).take(k)
    is.takeWhile(_ == true).size == k
  }
  def filter(es: Iterator[A]): Iterator[A] = es.filter(contains(_))
  def add(e: Any): Unit = {
    r.setSeed(e.hashCode)
    for (v <- Stream.continually(r.nextInt(w)).take(k)) bs.add(v)
  }
  def add(es: Seq[Any]): Unit = for (e <- es) add(e)
  def add(es: Iterator[Any]): Unit = for (e <- es) add(e)

  // -Utils---------
  lazy val accuracy: Double = {
    val exp = ((k: Double) * s) / w
    val p = Math.pow(1 - Math.exp(-exp), k)
    1d - p
  }
  lazy val info: String = s"Entries:$s, Width:$w, Keys:$k, Bits:${bs.size}"
  // -IO ---------
  def store(f: String) {
    val dos = new DataOutputStream(
      new BufferedOutputStream(new FileOutputStream(f))
    )
    dos.writeInt(s)
    dos.writeInt(w)
    dos.writeInt(k)
    println(s"Storing filter $info")
    for (l <- bs.toBitMask) dos.writeLong(l)
    dos.close
  }
}
// ---------
object BloomFilter {
  // def bestW( s:Int ):Int = 4 * s
  def bestW(s: Int): Int = 8 * s
  def optimalK(s: Int, w: Int): Int =
    Math.max((Math.round(9.0 / 13.0 * w / s)).toInt, 1)
  def hash(e: Any, its: Int, bounds: Int): Int = {
    Math.abs(
      if (its == 0) e.hashCode
      else its ^ hash(e, its - 1, bounds)
    ) % bounds
  }
  def load[A](f: String): BloomFilter[A] = {
    val dis = new DataInputStream(
      new BufferedInputStream(new FileInputStream(f))
    )
    val s = dis.readInt
    val w = dis.readInt
    val k = dis.readInt
    val st = Stream.continually(Try(dis.readLong)).takeWhile(_.isSuccess)
    val la = st
      .map(_ match {
        case Success(r) => r
        case _          => 0L
      })
      .toArray
    dis.close
    println("Loaded " + f + " " + s + " entries with k: " + k)
    val bs = BitSet.fromBitMask(la)
    new BloomFilter[A](s, w, k, bs)
  }
}
