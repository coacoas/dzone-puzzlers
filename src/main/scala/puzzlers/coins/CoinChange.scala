package puzzlers.coins

object CoinChange {
  def change(amount: Int) = { 
    def inc(m: Map[Int,Int], k: Int) = m.updated(k, m(k) + 1)
    
    def loop(acc: Map[Int, Int], coins: List[Int], remaining: Int): Vector[Map[Int,Int]] = { 
      if (remaining == 0) Vector(acc)
      else if (coins.isEmpty) Vector.empty
      else if (coins.head > remaining) loop(acc, coins.tail, remaining)
      else loop(inc(acc, coins.head), coins, remaining - coins.head) ++ 
           loop(acc, coins.tail, remaining)
    }
    loop(Map.empty.withDefaultValue(0), List(25, 10, 5, 1), amount)
  }
}