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
    Board(emptyBoard).isSquare should be (true)
  }
      
  test("A one-square board is square") { 
    Board(board0).isSquare should be (true)
  }
  
  test("A three-by-three board is square") { 
    Board(x3).isSquare should be (true)
  }
  
  test("A two-by-two board is square") { 
    Board(x2).isSquare should be (true)
  }
  
  test("A two-by-two's center board is empty") { 
    Board(x2).centerBoard.isEmpty should be (true)
  }
  
  test("An empty board is not an X") { 
    Board(emptyBoard).isX should be (false)
  }
  
  ignore("A two-by-two board of ones is not an X") { 
    Board(x2).isSquare should not be (true)
  }
  
  test("A three-by-two board is not square") { 
    Board(notSquare).isSquare should be (false)
  }
  
  test("A three-by-three square with an X is detected") {
    Board(x3).isX should be (true)
  }
  
  test("Not square board is not an X") { 
    Board(notSquare).isX should be (false)
  }
  
  test("A board with 1s in the corners only is not an X") { 
    Board(corners).isX should be (false)
  }
  
  test("Empty boards should have no immediate children") { 
    Board(emptyBoard).immediateSubBoards should equal (Stream.empty)
  }
  
  test("One-element boards have no immeidate children") { 
    Board(board0).immediateSubBoards.force.length should equal (0)
  }
  
  test("1 x 2 board has two immediate children") { 
    Board(Vector(Vector(1,1))).immediateSubBoards.force.length should equal (2)
  }
  
  test("2 x 1 board has two immediate children") { 
    Board(Vector(Vector(1), Vector(1))).immediateSubBoards.force.length should equal (2)
  }

  test("2 x 2 board has four immediate children") { 
    Board(Vector(Vector(0,1), Vector(1,0))).immediateSubBoards.map(b => (b.width, b.height)) should 
      equal (Stream((1,2),(2,1),(1,2),(2,1)))
  }
  
  test("Filtering already visited children") {
    val matrix = Vector(Vector(1,1))
    val b = Board(matrix)
    
    b.subBoardsSkipVisited(Set(Board(matrix, Position(1,0), 1, 1))).force should 
    equal (Stream(Board(matrix, Position(0, 0), 1, 1)))
  }
  
//  test("A 2x2 board with all children has nine elements") { 
//	val matrix = Vector(Vector(1,1),Vector(1,1))
//	val b = Board(matrix)
//    b.traverse.force.length should equal (9)
//  }
  
  test("Immediate sub-boards returns the correct values") { 
    val boards = Board(x3).immediateSubBoards
    boards.length should equal (4)
  }
}