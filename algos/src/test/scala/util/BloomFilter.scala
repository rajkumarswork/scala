package rajkumars.info.util

import rajkumars.info.util.IO

import org.scalatest.FlatSpec

class BloomFilterTest extends FlatSpec {

  val SourceFile = "/temp/data/enwiki.titles"
  val OutputFile = "/temp/test.bf"

  "BloomFilter" should "read input, create and store filter" in {
    assume(IO.fileExists(SourceFile))
    val size = io.Source.fromFile(SourceFile).getLines.size
    val wikeys = io.Source.fromFile(SourceFile).getLines.map(cleank(_))
    val startTS = System.currentTimeMillis
    val bf = new BloomFilter(wikeys, size)
    val creationTime = (System.currentTimeMillis - startTS) / 1000
    println(s"...done creating in $creationTime seconds. Storing...")
    bf.store(OutputFile)
  }

  "BloomFilter" should "output some performance statistics" in {
    assume(IO.fileExists(SourceFile))
    assume(IO.fileExists(OutputFile))
    val cf = BloomFilter.load[String](OutputFile)
    val N = 10000
    val somekeys =
      io.Source.fromFile(SourceFile).getLines.take(N)
    val badkeys = Seq("foo", "aar", "baz", "roo", "moo", "blue", "rue", "hoe")
    val testkeys = somekeys ++ badkeys
    val startTS = System.currentTimeMillis
    val r = for (s <- testkeys) yield cf(s)
    val lookupTime = System.currentTimeMillis - startTS
    println(f"\nExpected accuracy: ${cf.accuracy}%.3f")
    println(s"\n$N lookups took $lookupTime ms")
  }
  it should "output some lookup performance-results" in {
    assume(IO.fileExists(SourceFile))
    assume(IO.fileExists(OutputFile))
    val n = 1000000
    val ks = io.Source.fromFile(SourceFile).getLines.take(n)
    val bf = BloomFilter.load[String](OutputFile)
    val st = System.currentTimeMillis
    val good = bf.filter(ks).size
    val t = System.currentTimeMillis - st
    println(n + " checks took " + t + " ms (" + (n / t.max(1)) + " checks/ms)")
  }

  def cleank(s: String): String = {
    val pi = s.indexOf("(")
    if (pi > 0) s.substring(0, pi).trim
    else s
  }
}
