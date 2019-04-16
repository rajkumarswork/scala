package rajkumars.info.trees

import rajkumars.info.util.{IO, BloomFilter, Iterators}

// Helpers for LSMTrees implementation
case class Range(var low: Long = 0L, var high: Long = 0L) {
  def valid(v: Long): Boolean = v >= low && v <= high
  def update(v: Long) = { low = low.min(v); high = high.max(v) }
  def update(that: Range) = {
    low = low.min(that.low); high = high.max(that.high)
  }
  def update(it: Iterator[Range]): Unit = it.foreach(update(_))
  def overlap(that: Range): Boolean =
    if (that.high < low || that.low > high) false else true
  def all(): Range = { low = Long.MinValue; high = Long.MaxValue; this }
}

case class Rec[R](r: R, key: Int, time: Long = LSM.now(), del: Boolean = false)
    extends Ordered[Rec[R]] {
  override def compare(that: Rec[R]): Int = {
    val res = key.compare(that.key)
    if (res == 0) time.compare(that.time) else res
  }
  def valid(kr: Range, tr: Range): Boolean = kr.valid(key) && tr.valid(time)
}

case class EpochInfo(
    epoch: Int,
    size: Int,
    keyRange: Range,
    timeRange: Range,
    bf: BloomFilter[Int]
) {
  def valid(kr: Range, tr: Range): Boolean =
    kr.overlap(keyRange) && tr.overlap(timeRange)
}

object LSM {

  val MaxSize = 250000 // max number of in-memory records
  val MaxFiles = 16 // max-file before we compact them
  def now(): Long = System.currentTimeMillis

  val allTimes = Range(1500000000000L, 1650000000000L)
  val allKeys = Range().all()

  // - IO -
  val Prefix = "d"
  val MergePrefix = "m"

  def epochFileName(epoch: Int, dir: String): String = s"$dir/$Prefix.$epoch"
  def mergeFileName(e1: Int, e2: Int, dir: String): String =
    s"$dir/$MergePrefix.$e1.$e2"
  def epochFromDisk[R](e: Int, dir: String): Iterator[Rec[R]] =
    IO.fromFile[Rec[R]](epochFileName(e, dir))

  // returns only latest Rec
  def getFromEpoch[R](key: Int, epoch: Int, dir: String): Option[Rec[R]] =
    IO.fromFile[Rec[R]](epochFileName(epoch, dir))
      .filter(_.key == key)
      .take(1)
      .toSeq
      .headOption

  def readEpochRecords[R](
      epoch: Int,
      dir: String,
      kr: Range,
      tr: Range
  ): Iterator[Rec[R]] =
    IO.fromFile[Rec[R]](epochFileName(epoch, dir)).filter(_.valid(kr, tr))

  // merging
  def merge[R](e1: Int, e2: Int, dir: String): EpochInfo = {
    object KeyOrdering extends Ordering[Rec[R]] {
      def compare(a: Rec[R], b: Rec[R]) = a.key compare b.key
    }
    object TimeOrdering extends Ordering[Rec[R]] {
      def compare(a: Rec[R], b: Rec[R]) = b.time compare a.time
    }
    val epoch = if (e2 > e1) e2 else e1
    val it1 = IO.fromFile[Rec[R]](epochFileName(e1, dir))
    val it2 = IO.fromFile[Rec[R]](epochFileName(e2, dir))
    val it3 = Iterators.mergeDistinct[Rec[R]](List(it1, it2))
    val it4 = Iterators.distinct[Rec[R]](it3)(TimeOrdering)(KeyOrdering)
    val ofile = mergeFileName(e1, e2, dir)
    IO.toFile[Rec[R]](it4, ofile)
    epochInfo[R](ofile, epoch)
  }
  def epochInfo[R](fpath: String, epoch: Int): EpochInfo = {
    val size = IO.fromFile[Rec[R]](fpath).size
    val bf = new BloomFilter[Int](size)
    val (kr, tr) = (Range(), Range())
    for (r <- IO.fromFile[Rec[R]](fpath)) {
      bf.add(r)
      kr.update(r.key)
      tr.update(r.time)
    }
    EpochInfo(epoch, size, kr, tr, bf)
  }
}
