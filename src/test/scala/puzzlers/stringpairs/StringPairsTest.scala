package puzzlers.stringpairs

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.prop.PropertyChecks
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class StringPairsTest extends FunSpec with GivenWhenThen with Matchers with PropertyChecks {
  import StringPairs._
  val provided: Seq[String] = "dzone java dzone dzone javascript java".split(" ")

  describe("Adjacent Pairs") {
    it("should detect adjacent pairs") {
      Given("the example")
      When("adjacent pairs are found")
      val adj = adjacentPairs(provided)
      Then("it should find one adjacent pair of 'dzone'")
      adj should equal(Seq("dzone"))
    }
    it("should never find adjacent pairs in one-word lists") {
      forAll { (s: String) =>
        adjacentPairs(Seq(s)) should be('empty)
      }
    }
    it("should return one element for two-element lists of the same value") {
      forAll { (s: String) =>
        adjacentPairs(Seq(s, s)) should be(Seq(s))
      }
    }
    it("should not find adjacent pairs if there is anything in between") {
      forAll { (same: String, diff: String) =>
        whenever(same != diff) {
          adjacentPairs(Seq(same, diff, same)) should be('empty)
        }
      }
    }
    it("finds two pairs if two adjacent pairs exist") {
      forAll { (s1: String, s2: String) =>
        whenever(s1 != s2) {
          adjacentPairs(Seq(s1, s1, s2, s2)) should be(Seq(s1, s2))
        }
      }
    }
    it("finds matches in the middle of a sequence") {
      forAll { (s1: String, s2: String) =>
        whenever(s1 != s2) {
          adjacentPairs(Seq(s2, s1, s1, s2)) should be(Seq(s1))
        }
      }
    }
    it("finds three-in-a-row as two adjacent pairs") {
      forAll { (s1: String) =>
        adjacentPairs(Seq(s1, s1, s1)) should equal(Seq(s1, s1))
      }
    }
  }

  describe("Combination pairs - not-necessarily-consecutive pairs") {
    it("should not exist in a one-element list") {
      forAll { (s: String) =>
        combinations(Seq(s)) should be('empty)
      }
    }

    it("should find one combination in a list with two of the same element") {
      forAll { (s: String) =>
        combinations(Seq(s, s)) should be(Map(s -> Set((0, 1))))
      }
    }

    it("should find one combination when there is a different string between two of the same string") {
      forAll { (s1: String, s2: String) =>
        whenever(s1 != s2) {
          combinations(Seq(s1, s2, s1)) should be(Map(s1 -> Set((0, 2))))
        }
      }
    }

    it("should find two combinations when two strings alternate") {
      forAll { (s1: String, s2: String) =>
        whenever(s1 != s2) {
          combinations(Seq(s1, s2, s1, s2)) should equal(Map(s1 -> Set((0, 2)), s2 -> Set((1, 3))))
        }
      }
    }

    it("should find three combinations for three-in-a-row") {
      forAll { (s: String) =>
        combinations(Seq(s, s, s)) should equal(Map(s -> Set((0, 1), (0, 2), (1, 2))))
      }
    }
    
    it("should find three combinations for three of the same element in a list, separated by different strings") { 
      forAll { (s: String, s1: String, s2: String) => 
        whenever(s != s1 && s1 != s2 && s != s2) {
          combinations(Seq(s, s1, s, s2, s)) should equal(Map(s -> Set((0,2),(0,4),(2,4))))  
        }
      }
    }
    
    it("Can find more than one set of combinations") { 
      forAll { (s1: String, s2: String) => 
        whenever (s1 != s2) { 
          combinations(Seq(s1,s2,s1,s2,s1)) should equal (Map(s1 -> Set((0,2),(0,4),(2,4)), s2 -> Set((1,3))))
        }  
      }
    }
  }
}