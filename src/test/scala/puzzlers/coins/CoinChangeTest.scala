/**
 * <p>
 * Copyright (c) 2001, 2013, Object Innovation, Inc. All Rights Reserved.
 *
 * This software is published under the terms of the Object Innovation License
 * version 1.1, a copy of which has been included with this distribution in the
 * LICENSE.TXT file. Learn more at http://www.bridgegatetei.com
 * </p>
 */

package puzzlers.coins

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
/**
 * @author bcarlson
 *
 */
@RunWith(classOf[JUnitRunner])
class CoinChangeTest extends FunSuite with Matchers with PropertyChecks {
  test("5c should get change of 1 x 5 and 5 x 1") { 
    CoinChange.change(5) should equal (Vector(Map(5 -> 1), Map(1 -> 5)))
  }
  
  test("37c should get... " ) {
    CoinChange.change(37) should equal (Vector(
        Map(25 -> 1, 10 -> 1, 1 -> 2),
        Map(25 -> 1, 5 -> 2, 1 -> 2),
        Map(25 -> 1, 5 -> 1, 1 -> 7),
        Map(25 -> 1, 1 -> 12),
        Map(10 -> 3, 5 -> 1, 1 -> 2),
        Map(10 -> 3, 1 -> 7),
        Map(10 -> 2, 5 -> 3, 1 -> 2),
        Map(10 -> 2, 5 -> 2, 1 -> 7),
        Map(10 -> 2, 5 -> 1, 1 -> 12),
        Map(10 -> 2, 1 -> 17),
        Map(10 -> 1, 5 -> 5, 1 -> 2),
        Map(10 -> 1, 5 -> 4, 1 -> 7),
        Map(10 -> 1, 5 -> 3, 1 -> 12),
        Map(10 -> 1, 5 -> 2, 1 -> 17),
        Map(10 -> 1, 5 -> 1, 1 -> 22),
        Map(10 -> 1, 1 -> 27),
        Map(5 -> 7, 1 -> 2),
        Map(5 -> 6, 1 -> 7), 
        Map(5 -> 5, 1 -> 12),
        Map(5 -> 4, 1 -> 17),
        Map(5 -> 3, 1 -> 22), 
        Map(5 -> 2, 1 -> 27), 
        Map(5 -> 1, 1 -> 32),
        Map(1 -> 37)))
  }
  
  test("Coin change combinations are unique")  {
    forAll(Gen.choose(1,100)) { (amount: Int) => 
      val change = CoinChange.change(amount)
      change.size should equal (change.toSet.size)
    }
  }
  
  test("All change counts should add up to the total") { 
    forAll(Gen.choose(1,100)) { (amount: Int) => 
      val changes = CoinChange.change(amount)
      def total(m: Map[Int,Int]) = m.toList.map(e => e._2 * e._1).sum
      changes.map(total).foreach { 
        _ should equal (amount)
      }
    }
  }
}