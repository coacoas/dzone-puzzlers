package puzzlers.coins

object CoinChange {
  val denominations = Vector(25, 10, 5, 1)
  
  def change(amount: Int) = { 
    def update[K,V](m: Map[K,V], k: K)(f: V => V) = m.updated(k, f(m(k)))
    def inc(m: Map[Int,Int], k: Int) = update(m, k)(_ + 1)
    
    def loop(acc: Map[Int, Int], coins: Vector[Int], remaining: Int): Vector[Map[Int,Int]] = { 
      if (remaining == 0) Vector(acc)
      else if (coins.isEmpty) Vector.empty
      else if (coins.head > remaining) loop(acc, coins.tail, remaining)
      else loop(inc(acc, coins.head), coins, remaining - coins.head) ++ 
           loop(acc, coins.tail, remaining)
    }
    loop(Map.empty.withDefaultValue(0), denominations, amount)
  }
}