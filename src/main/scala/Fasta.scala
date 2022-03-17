class Fasta(val name: String, val dna: String) {
  def gcContent(): Double = {
    (dna.count(_ == 'G') + dna.count(_ == 'C')) / dna.length.toDouble
  }
}

object Fasta {

  def parseFastas(s: String): List[Fasta] = {
    //fUnCtIoNaL pRoGrAmMiNg
    for (fasta <- s.split(">").tail) yield new Fasta(fasta.split("\n").head, fasta.split("\n").tail.mkString(""))
  }.toList
}