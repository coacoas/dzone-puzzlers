package puzzlers.simple

object SimpleNumberPrinting extends App { 
  val hasRepeating = (cs: List[Char]) => cs.sliding(2).exists(csprime => csprime(0) == csprime(1))
  
  val rangeNoRepeating = (45 to 4578).map(_.toString).map(_.toList).filterNot(SimpleNumberPrinting.hasRepeating)
  
  println(rangeNoRepeating.map(_.mkString).mkString("/"))
  println(s"Found ${rangeNoRepeating.length} elements")
}