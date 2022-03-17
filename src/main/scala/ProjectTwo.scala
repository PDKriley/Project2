import scala.annotation.tailrec

object ProjectTwo {
  def translateRna(s: String): String = {
    for (codon <- s.grouped(3)) yield if(CodonTable.getAminoAcid(codon) == "Stop") "" else CodonTable.getAminoAcid(codon)
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

  def profile(ss: Seq[String]): Seq[Map[Char, Int]] = {
    for (col <- ss.transpose) yield col.groupBy((c: Char) => c).map(el => (el._1, el._2.length))
  }

}