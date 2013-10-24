package puzzlers.sudoku

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.GivenWhenThen

@RunWith(classOf[JUnitRunner])
class SudokuTest extends FunSpec with Matchers with GivenWhenThen {
  describe("A Sudoku Board") {
    import TestData._
    it("should return the rows properly") {
      new Matrix(correct).rows should equal(correct)
    }
    it("should return columns") {
      val columns = Vector(
        Vector(3, 6, 2, 9, 7, 8, 5, 4, 1),
        Vector(5, 7, 4, 3, 1, 6, 2, 9, 8),
        Vector(8, 1, 9, 5, 2, 4, 6, 7, 3),
        Vector(2, 5, 8, 4, 6, 9, 3, 1, 7),
        Vector(1, 9, 3, 7, 5, 2, 4, 8, 6),
        Vector(6, 4, 7, 8, 3, 1, 9, 2, 5),
        Vector(4, 3, 6, 2, 8, 7, 1, 5, 9),
        Vector(7, 2, 1, 6, 9, 5, 8, 3, 4),
        Vector(9, 8, 5, 1, 4, 3, 7, 6, 2))

      new Matrix(correct).columns should equal(columns)
    }
    it("should be able to retrieve regions") {
      Given("A correct matrix")
      val regions = new Matrix(correct).regions(9)
      Then("There should be nine regions")
      regions.length should be(9)
      And("Each region should have nine elements")
      regions.foreach(_.length should equal(9))
      And("They should be split properly")
      regions should equal(
        Vector(
          new Matrix(Vector(Vector(3, 5, 8), Vector(6, 7, 1), Vector(2, 4, 9))),
          new Matrix(Vector(Vector(2, 1, 6), Vector(5, 9, 4), Vector(8, 3, 7))),
          new Matrix(Vector(Vector(4, 7, 9), Vector(3, 2, 8), Vector(6, 1, 5))),
          new Matrix(Vector(Vector(9, 3, 5), Vector(7, 1, 2), Vector(8, 6, 4))),
          new Matrix(Vector(Vector(4, 7, 8), Vector(6, 5, 3), Vector(9, 2, 1))),
          new Matrix(Vector(Vector(2, 6, 1), Vector(8, 9, 4), Vector(7, 5, 3))),
          new Matrix(Vector(Vector(5, 2, 6), Vector(4, 9, 7), Vector(1, 8, 3))),
          new Matrix(Vector(Vector(3, 4, 9), Vector(1, 8, 2), Vector(7, 6, 5))),
          new Matrix(Vector(Vector(1, 8, 7), Vector(5, 3, 6), Vector(9, 4, 2)))))
    }
  }

  val sum = (1 to 9).sum
  describe("A correct Sudoku play") {
    val correct = new Matrix(TestData.correct)
    Given("A correctly filled out sudoku")
    Then(s"All rows should sum to ${sum}")
    correct.rows.foreach { _.toSet.sum should equal(sum) }
    And(s"All columns should sum to ${sum}")
    correct.columns.foreach { _.toSet.sum should equal(sum) }
    And(s"All regions should sum to ${sum}")
    correct.regions(9).foreach { _.toSet.sum should equal(sum) }
    And("it should be valid")
    correct.check should be(true)
  }

  describe("A Sudoku board with a row error") {
    val rowError = new Matrix(TestData.rowError)
    it("Should check as false") {
      rowError.check should be(false)
    }
  }
}

object TestData {
  // Correct
  val correct = Vector(
    Vector(3, 5, 8, 2, 1, 6, 4, 7, 9),
    Vector(6, 7, 1, 5, 9, 4, 3, 2, 8),
    Vector(2, 4, 9, 8, 3, 7, 6, 1, 5),
    Vector(9, 3, 5, 4, 7, 8, 2, 6, 1),
    Vector(7, 1, 2, 6, 5, 3, 8, 9, 4),
    Vector(8, 6, 4, 9, 2, 1, 7, 5, 3),
    Vector(5, 2, 6, 3, 4, 9, 1, 8, 7),
    Vector(4, 9, 7, 1, 8, 2, 5, 3, 6),
    Vector(1, 8, 3, 7, 6, 5, 9, 4, 2))

  // Regions Error
  val regionError = Vector(
    Vector(3, 5, 8, 2, 1, 6, 4, 7, 9),
    Vector(6, 7, 1, 5, 9, 4, 3, 2, 8),
    Vector(9, 3, 5, 4, 7, 8, 2, 6, 1),
    Vector(2, 4, 9, 8, 3, 7, 6, 1, 5),
    Vector(7, 1, 2, 6, 5, 3, 8, 9, 4),
    Vector(8, 6, 4, 9, 2, 1, 7, 5, 3),
    Vector(5, 2, 6, 3, 4, 9, 1, 8, 7),
    Vector(4, 9, 7, 1, 8, 2, 5, 3, 6),
    Vector(1, 8, 3, 7, 6, 5, 9, 4, 2))

  // Columns Error
  val columnError = Vector(
    Vector(3, 5, 8, 2, 1, 6, 4, 7, 9),
    Vector(7, 6, 1, 5, 9, 4, 3, 2, 8),
    Vector(2, 4, 9, 8, 3, 7, 6, 1, 5),
    Vector(9, 3, 5, 4, 7, 8, 2, 6, 1),
    Vector(7, 1, 2, 6, 5, 3, 8, 9, 4),
    Vector(8, 6, 4, 9, 2, 1, 7, 5, 3),
    Vector(5, 2, 6, 3, 4, 9, 1, 8, 7),
    Vector(4, 9, 7, 1, 8, 2, 5, 3, 6),
    Vector(1, 8, 3, 7, 6, 5, 9, 4, 2))

  // Rows Error
  val rowError = Vector(
    Vector(3, 5, 8, 2, 1, 6, 4, 7, 9),
    Vector(6, 7, 1, 5, 9, 4, 3, 2, 8),
    Vector(2, 4, 9, 8, 3, 7, 6, 1, 5),
    Vector(9, 3, 5, 4, 7, 8, 2, 6, 1),
    Vector(7, 1, 2, 6, 5, 3, 8, 9, 4),
    Vector(8, 6, 4, 9, 2, 1, 7, 5, 3),
    Vector(5, 2, 6, 3, 4, 2, 1, 8, 7),
    Vector(4, 9, 7, 1, 8, 9, 5, 3, 6),
    Vector(1, 8, 3, 7, 6, 5, 9, 4, 2))
}
