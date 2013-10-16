/**
 * <p>
 * Copyright (c) 2001, 2013, Object Innovation, Inc. All Rights Reserved.
 *
 * This software is published under the terms of the Object Innovation License
 * version 1.1, a copy of which has been included with this distribution in the
 * LICENSE.TXT file. Learn more at http://www.bridgegatetei.com
 * </p>
 */

package puzzlers.kpalindrome

import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalacheck.Gen

/**
 * @author bcarlson
 *
 */
class KPalindromeTest extends FunSuite with Matchers with PropertyChecks {
  import KPalindrome._
  val positiveK = Gen.choose(0, 10)
  val nonEmptyString = for (s <- Gen.alphaStr if !(s.isEmpty)) yield s
  
  test("An empty string and its reverse have no differences") { 
    differences("", "".reverse) should be ('empty)
  }
  
  test("An empty string is a palindrome") { 
    isKPalindrome(0)("") should be (true)
  }
  
  test("A single-character string is always a palindrome") {
    forAll(positiveK, Gen.alphaNumChar) { (n: Int, c: Char) =>  
      isKPalindrome(n)(c.toString) should be (true)
    }
  }
 
  test("1-Palindrome positive tests") { 
    val onePalindromes = Table(("s"), 
        ("radar"),
        ("badar"),
        ("radir"),
        ("aaabaa"),
        ("amanaplanacanalpanama"),
        ("amaqaplanacanalpaxama"),
        ("amamaplanacanalpanama"))
    forAll(onePalindromes) { (s: String) =>  
      isKPalindrome(1)(s) should be (true)
    }
  }

  test("1-Palindrome negative tests") { 
    val onePalindromes = Table(("s"), 
        ("radum"),
        ("bizarre"),
        ("bazaar"),
        ("amanaplanacanalsuez"))
    forAll(onePalindromes) { (s: String) =>  
      isKPalindrome(1)(s) should be (false)
    }
  }

  test("2-Palindrome") { 
    val onePalindromes = Table(("s"), 
        ("radar"),
        ("badar"),
        ("radir"),
        ("amanaplanacanalpanama"),
        ("amaqaplanacanalpaxama"),
        ("amamaplanacanalpanama"))
    forAll(onePalindromes) { (s: String) =>  
      isKPalindrome(2)(s) should be (true)
    }
  }
  
  test("All strings are at least an n-palindrome where n = string length / 2") { 
    forAll{ (s: String) => 
      isKPalindrome(s.length/2)(s) should be (true)
    }
  }
}