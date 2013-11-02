package puzzlers.coins

object CoinChange {
  val denominations = Vector(25, 10, 5, 1)
  
  def change(amount: Int) = { 
    def update[K,V](m: Map[K,V], k: K)(f: V => V) = m.updated(k, f(m(k)))
    def inc(m: Map[Int,Int], k: Int) = update(m, k)(_ + 1)
    
    def loop(acc: Map[Int, Int], remainingDenominations: Vector[Int], remainingAmount: Int): Vector[Map[Int,Int]] = { 
      if (remainingAmount == 0) Vector(acc)
      else if (remainingDenominations.isEmpty) Vector.empty
      else if (remainingDenominations.head > remainingAmount) loop(acc, remainingDenominations.tail, remainingAmount)
      else { 
        val d = remainingDenominations.head
        loop(inc(acc, d), remainingDenominations, remainingAmount - d) ++ loop(acc, remainingDenominations.tail, remainingAmount)
      }
    }
    loop(Map.empty.withDefaultValue(0), denominations, amount)
  }
}