package rajkumars.info.trees

import rajkumars.info.util.{IO, BloomFilter}

import scala.collection.mutable.SortedMap

// Implementation of a Log-Structured Merge-Tree with BloomFilters
// Notes:
// 1) If thread-safety is needed, modify to use for data
//    java.util.concurrent.ConcurrentHashMap (and sort before flushing to disk)
// 2) Might need a different mechanism to trigger flush-to-disk and consolidate-files
//    Is possibe that add(iterator) is never called
class LSMTree[R](dir: String) extends Serializable {

  var epoch: Int = 0
  val data = SortedMap[Int, Rec[R]]()
  var epochs = SortedMap[Int, EpochInfo]()(Ordering.Int.reverse)

  // The api
  def add(key: Int, r: R): Unit = synchronized { data(key) = Rec[R](r, key) }
  def add(r: R): Unit = add(r.hashCode, r)
  def add(rit: Iterator[R]): Unit = synchronized {
    val t = LSM.now()
    for (r <- rit) {
      val key = r.hashCode
      data(key) = Rec[R](r, key, t, false)
    }
    if (data.size > LSM.MaxSize) flushToDisk()
    if (epochs.size > LSM.MaxFiles) consolidate()
  }

  def remove(key: Int, r: R): Unit = data(key) = Rec[R](r, key, LSM.now(), true)
  def remove(r: R): Unit = remove(r.hashCode, r)

  def exists(r: R): Boolean = exists(r.hashCode)
  def exists(key: Int): Boolean = {
    if (data.contains(key)) !data(key).del
    else fromFile(key).isDefined
  }

  def scan(
      keyRange: Range = LSM.allKeys,
      timeRange: Range = LSM.allTimes
  ): Iterator[R] =
    readRecords(keyRange, timeRange).map(_.r)

  // ----------------------------------
  // - privates ---
  // - cache helpers
  private def validCacheRecords(kr: Range, tr: Range): Iterator[Rec[R]] =
    data.values.filter(_.valid(kr, tr)).toIterator

  private def cacheRange(key: Boolean = true): Range = {
    val r = Range()
    val vals = if (key) data.keys.map(_.toLong) else data.values.map(_.time)
    vals.foreach(r.update(_))
    r
  }

  // - file helpers ---
  private def getRange(key: Boolean = true): Range = {
    val r = Range()
    r.update(
      epochs.values.map(e => if (key) e.keyRange else e.timeRange).toIterator
    )
    r.update(cacheRange(key))
    r
  }

  // - merges --
  def consolidate(): Unit =
    epochs.keys
      .sliding(2, 2)
      .foreach(l => if (l.size == 2) merge(l.head, l.last))

  def merge(smallEpoch: Int, bigEpoch: Int): Boolean = {
    val newEpochInfo = LSM.merge(smallEpoch, bigEpoch, dir)
    switch(smallEpoch, bigEpoch, newEpochInfo)
  }
  private def switch(
      smallEpoch: Int,
      bigEpoch: Int,
      eInfo: EpochInfo
  ): Boolean = synchronized {
    val newfile = LSM.mergeFileName(smallEpoch, bigEpoch, dir)
    val oldfile = LSM.epochFileName(bigEpoch, dir)
    IO.fileRename(oldfile, oldfile + ".save")
    if (IO.fileRename(newfile, oldfile)) {
      epochs(bigEpoch) = eInfo
      epochs.remove(smallEpoch)
      true
    } else false
  }

  // Doesn't need sync{} as the adds have them, but here in case we choose to use concurrent-maps
  def flushToDisk() = synchronized {
    IO.toFile[Rec[R]](data.values.toIterator, LSM.epochFileName(epoch, dir))
    val bf = new BloomFilter(data.keys.toIterator, data.size)
    epochs(epoch) = EpochInfo(
      epoch,
      data.size,
      cacheRange(key = true),
      cacheRange(key = false),
      bf
    )
    epoch = epoch + 1
    data.clear
  }
  def getValidEpochs(kr: Range, tr: Range): Iterable[Int] =
    epochs.collect { case (e, ed) if ed.valid(kr, tr) => e }

  def readRecords(kr: Range, tr: Range): Iterator[Rec[R]] = {
    val eit: Iterator[Int] = getValidEpochs(kr, tr).toIterator
    val epochRecords =
      eit.map(e => LSM.readEpochRecords[R](e, dir, kr, tr)).flatten.toIterator
    val cacheRecords =
      validCacheRecords(getRange(key = true), getRange(key = false))
    cacheRecords ++ epochRecords
  }
  private def fromFile(key: Int): Option[Rec[R]] =
    epochsWithKey(key)
      .map(e => LSM.getFromEpoch[R](key, e, dir))
      .filter(_.isDefined)
      .flatten
      .headOption

  // Earliest n epochs with key
  private def epochsWithKey(key: Int): Iterable[Int] =
    epochs.collect { case (e, ed) if (ed.bf.contains(key)) => e }
}
