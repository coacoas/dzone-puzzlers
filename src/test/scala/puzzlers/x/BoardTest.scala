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
}