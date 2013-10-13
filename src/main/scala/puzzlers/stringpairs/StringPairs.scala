package puzzlers.stringpairs

object StringPairs {
  def adjacentPairs(strings: Seq[String]): List[String] = 
    if (strings.size > 1) strings.sliding(2).filter(xs => xs(0) == xs(1)).map(_(0)).toList
    else List()
  
  def combinations(strings: Seq[String]): Map[String, Set[(Int, Int)]] = 
    strings.
    zipWithIndex.
    groupBy(_._1).
    filter(_._2.length > 1).
    mapValues(_.map(_._2)).
    mapValues(_.combinations(2).toVector.map(x => ( x(0), x(1))).toSet)  
}