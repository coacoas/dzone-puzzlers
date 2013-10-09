package puzzlers.x

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner])
class BoardTest extends FunSuite with Matchers {
  val emptyBoard = Vector(Vector[Int]())
  val board0 = Vector(
      Vector(0))
  val board1 = Vector(
      Vector(1))
  
  val x2 = Vector(
      Vector(1,1),
      Vector(1,1))
  val x3 = Vector(
      Vector(1,0,1),
      Vector(0,1,0),
      Vector(1,0,1))

  val corners = Vector(
      Vector(1,0,1),
      Vector(0,0,0),
      Vector(1,0,1))

  val notSquare = Vector(
      Vector(0,1,0),
      Vector(1,0,1))
      
  test("An empty board is square") { 
    Matrix(emptyBoard).isSquare should be (true)
  }
      
  test("A one-square board is square") { 
    Matrix(board0).isSquare should be (true)
  }
  
  test("A three-by-three board is square") { 
    Matrix(x3).isSquare should be (true)
  }
  
  test("A two-by-two board is square") { 
    Matrix(x2).isSquare should be (true)
  }
  
  test("An empty board is not an X") { 
    Matrix(emptyBoard).isX should be (false)
  }
  
  test("A two-by-two board of ones is not an X") { 
    Matrix(x2).isX should be (false)
  }
  
  test("A three-by-two board is not square") { 
    Matrix(notSquare).isSquare should be (false)
  }
  
  test("A three-by-three square with an X is detected") {
    Matrix(x3).isX should be (true)
  }
  
  test("Not square board is not an X") { 
    Matrix(notSquare).isX should be (false)
  }
  
  test("A board with 1s in the corners only is not an X") { 
    Matrix(corners).isX should be (false)
  }
  
  test("Empty boards should have no immediate children") { 
    Matrix(emptyBoard).immediateSubMatrices should equal (Stream.empty)
  }
  
  test("One-element boards have no immeidate children") { 
    Matrix(board0).immediateSubMatrices.length should equal (0)
  }
  
  test("2 x 1 board has two immediate children") { 
    val m = Vector(Vector(1,1))
    Matrix(m).legalImmediateSubMatrices.toSet should equal (Set(
        SingleElementMatrix(m, Position(1,0)), 
        SingleElementMatrix(m, Position(0,0))))
  }
  
  test("1 x 2 board has two immediate children") { 
    val m = Vector(Vector(1), Vector(1))
    Matrix(m).legalImmediateSubMatrices.toSet should equal (Set(
        SingleElementMatrix(m, Position(0,0)),
        SingleElementMatrix(m, Position(0,1))))
  }

  test("2 x 2 board has four immediate children") { 
    Matrix(Vector(Vector(0,1), Vector(1,0))).legalImmediateSubMatrices.collect { case b: LargeMatrix => 
      (b.width, b.height)
    } should equal (Vector((1,2),(2,1),(1,2),(2,1)))
  }
  
  test("Filtering already visited children") {
    val matrix = Vector(Vector(1,1))
    val b = Matrix(matrix)
    
    b.subMatricesSkipVisited(Set(Matrix(matrix, Position(1,0), 1, 1))) should 
    equal (Vector(Matrix(matrix, Position(0, 0), 1, 1)))
  }
  
  test("A 2x2 board with all children has nine elements") { 
	val matrix = Vector(Vector(1,1),Vector(1,1))
	val b = Matrix(matrix)
    b.traverse.length should equal (9)
  }
  
  test("Immediate sub-boards returns the correct values") { 
    val boards = Matrix(x3).immediateSubMatrices
    boards.length should equal (4)
  }
  
  test("Find the largest X") {
    val m = Vector(
        Vector(0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0),
        Vector(0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0),
        Vector(0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0),
        Vector(0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,0,1,0),
        Vector(0,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0),
        Vector(1,0,1,1,1,1,0,0,1,0,1,0,1,0,1,0,1,0),
        Vector(1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0),
        Vector(0,1,0,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0),
        Vector(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0))
    
    val largestX = Matrix(m).largestX
    Matrix(m).largestX should equal (Some(SquareMatrix(m, Position(2, 2), 5)))
  }
  
  test("All one X") { 
    val m = Vector(
        Vector(1,0,1,0,1),
        Vector(0,1,1,1,0),
        Vector(1,1,1,1,1),
        Vector(0,1,1,1,0),
        Vector(1,0,1,0,1))
    Matrix(m).largestX should equal(Some(SquareMatrix(m, Position(0,0),5)))
  }
  
  test("Has no largest X") { 
    val m = Vector(
        Vector(0,1,0),
        Vector(1,0,1))
    Matrix(m).largestX should be (None)
  }
}