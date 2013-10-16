package puzzlers.kpalindrome

object KPalindrome {
  implicit val defaultChar = 30.toChar

  def differences[T](s1: Seq[T], s2: Seq[T])(implicit default: T): Seq[(T,T)] = 
    s1.zipAll(s2, default, default).collect { case (a, b) if a != b => (a,b) }
  
  
  def isKPalindrome(k: Int)(s: String) = differences(s, s.reverse).length <= (k * 2)
}