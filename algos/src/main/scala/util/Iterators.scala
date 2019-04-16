package rajkumars.info.util

// Iterator tools
object Iterators {

  // merge ordered iterators (preserving order)
  def merge[T](
      rawIterators: List[Iterator[T]]
  )(implicit cmp: Ordering[T]): Iterator[T] = {
    new Iterator[T] {
      private val iterators: List[BufferedIterator[T]] =
        rawIterators.map(_.buffered)

      def hasNext: Boolean = iterators.exists(_.hasNext)

      def next(): T =
        if (hasNext)
          iterators
            .filter(_.hasNext)
            .map(x => (x.head, x))
            .minBy(_._1)(cmp)
            ._2
            .next()
        else {
          throw new Exception
        }
    }
  }

  // merge ordered iterators eliminate duplicates
  def mergeDistinct[T](
      rawIterators: List[Iterator[T]]
  )(implicit cmp: Ordering[T]): Iterator[T] = {
    var last: Option[T] = None
    new Iterator[T] {
      private val iterators: List[BufferedIterator[T]] =
        rawIterators.map(_.buffered)

      def hasNext: Boolean = {
        val its = iterators.filter(_.hasNext)
        if (its.isEmpty) false
        else {
          val minit = its.map(x => (x.head, x)).minBy(_._1)(cmp)._2
          Some(minit.head) match {
            case v if (v != last) => true
            case v if (v == last) => { minit.next; hasNext }
          }
        }
      }

      def next(): T = {
        if (hasNext) {
          last = Some(
            iterators
              .filter(_.hasNext)
              .map(x => (x.head, x))
              .minBy(_._1)(cmp)
              ._2
              .next()
          )
          last.get
        } else {
          throw new Exception
        }
      }
    }
  }

  // get distinct elements from ordered iterator
  // picker specifies which of the similar elements to pick
  def distinct[T](
      sit: Iterator[T]
  )(picker: Ordering[T])(implicit cmp: Ordering[T]): Iterator[T] = {

    var last: Option[T] = None
    new Iterator[T] {
      val bit = sit.buffered

      def hasNext: Boolean = bit.hasNext
      def next(): T =
        if (hasNext) {
          val save = last
          last = Some(bit.next)
          if (save.isDefined && last.isDefined && cmp.compare(
                save.get,
                last.get
              ) == 0) {
            last = if (picker.compare(save.get, last.get) < 0) save else last
          }
          bit match {
            case mit if (!mit.hasNext)                         => last.get
            case mit if (cmp.compare(last.get, mit.head) != 0) => last.get
            case mit if (cmp.compare(last.get, mit.head) == 0) => next()
          }
        } else throw new Exception
    }
  }

  // creates n duplicates of an iterator
  // Warning: the data between the earliest and latest iterator is kept in memory
  def dupn[T](it: Iterator[T], n: Int): Seq[Iterator[T]] =
    n match {
      case 0 => Seq[Iterator[T]]()
      case 1 => Seq[Iterator[T]](it)
      case 2 => val (a, b) = it.duplicate; Seq[Iterator[T]](a, b)
      case r =>
        val s = dupn[T](it, r - 1); val (a, b) = s.head.duplicate;
        s.tail ++ Seq[Iterator[T]](a, b)
    }
}
