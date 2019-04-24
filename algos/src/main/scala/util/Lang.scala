package rajkumars.info.util

object Lang {

  val Base = 31
  // rolling hash (i.e. hashes of windowed-substrings of s, of size n). O(s)
  def rollingHash(s: String, n: Int): Seq[Int] = {
    val fallingMul:Int = Math.pow( 31, n).toInt  // n, not (n-1) as multiply by Base 'fore minus
    val start: Int = hashCode( s.take( n ))
    val rest = s.drop( n )
    val pairs: Seq[(Char,Char)] = s.toSeq.zip( rest.toSeq )
    pairs.scanLeft(start){ case (hash,(d,a)) => updateHash( hash, d, a, fallingMul )}
  }

  def updateHash( hash: Int, drop: Char, add: Char, mul: Int): Int =
    hash * Base  - drop.hashCode  * mul + add.hashCode

  // scala-js hashCode implementation for strings
  def hashCode(s: String): Int = {
    var res = 0
    var mul = 1 // holds pow(31, length-i-1)
    var i = s.length-1
    while (i >= 0) {
      res += s.charAt(i) * mul
      mul *= Base
      i -= 1
    }
    res
  }

  // Rabin-Karp O(s) substring-search: returns index of first match, else -1
  def substring( s: String, n: String ): Int = {
    val nHash = n.hashCode
    val indices = rollingHash( s, n.size ).zipWithIndex.collect{ case(h,i) if( h == nHash) => i }
    val matches = indices.filter( i => s.substring( i, i+n.size ) == n )
    if( matches.size > 0 ) matches.head else -1
  }

}
