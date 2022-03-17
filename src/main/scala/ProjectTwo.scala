import scala.annotation.tailrec

object ProjectTwo {
  def translateRna(s: String): String = {
    val groups = s.grouped(3).toList
    for (codon <- groups) yield if(CodonTable.getAminoAcid(codon) == "Stop") "" else CodonTable.getAminoAcid(codon)
  }.mkString

  def countSources(rna: String): Int = {

    @tailrec
    def helper(seq: String, mod: Int): Int = {
      seq match {
        case "" => mod * 3 % 1000000
        case c: String => helper(seq.tail, mod * CodonTable.numOccurrences(c.head) % 1000000)
      }
    }

    helper(rna, 1)
  }
}