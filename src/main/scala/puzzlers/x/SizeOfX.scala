package puzzlers.x

import scala.annotation.tailrec

case class Position(val x: Int, val y: Int) {
  def +(other: Position) = Position(x + other.x, y + other.y)
  def -(other: Position) = Position(x - other.x, y - other.y)
}

sealed trait Matrix {
  def isValid: Boolean
  def isSquare: Boolean
  def isX: Boolean
  def traverse: Stream[Matrix]
  def centerBoard: Matrix
  def immediateSubBoards: Seq[Matrix]
  def legalImmediateSubBoards: Seq[Matrix] = immediateSubBoards.filter(_.isValid)
  def subBoardsSkipVisited(visited: Set[Matrix]): Seq[Matrix] = legalImmediateSubBoards.filterNot(visited)
  def largestX: Option[SquareMatrix] = traverse.collect({ case square: SquareMatrix => square}).find(_.isX)
}

case object EmptyMatrix extends Matrix {
  override val isValid = false
  override val isSquare = true
  override val isX = false
  override val centerBoard = EmptyMatrix
  override val traverse = Stream.empty
  override val immediateSubBoards = Vector.empty
}

abstract class BaseMatrix(matrix: Vector[Vector[Int]]) {
  this: Matrix =>
  def valueAt(p: Position): Option[Int] = try {
    Some(matrix(p.y)(p.x))
  } catch {
    case _: Exception => None
  }

}

case class SingleElementMatrix(matrix: Vector[Vector[Int]], position: Position) extends BaseMatrix(matrix) with Matrix {
  override def isValid = valueAt(position).isDefined
  override val isSquare = true
  override val isX = valueAt(position) == Some(1)
  override val centerBoard = EmptyMatrix
  override val traverse = Stream.empty
  override val immediateSubBoards = Vector.empty
}

case class SquareMatrix(matrix: Vector[Vector[Int]], upperLeft: Position, size: Int) extends LargeBoard(matrix, upperLeft, size, size) {
  override def isValid = (upperLeft.y + size) < matrix.length
  override val isSquare = true
  override def centerBoard = Matrix(matrix, upperLeft + Position(1, 1), size - 2, size - 2)
  override def isX = corners.forall(_ == Some(1)) && centerBoard.isX
}

case class NonSquareBoard(matrix: Vector[Vector[Int]], upperLeft: Position, override val width: Int, override val height: Int) extends LargeBoard(matrix, upperLeft, width, height) {
  override def isValid = (upperLeft.y + height) <= matrix.length && (upperLeft.x + width) <= matrix.head.length
  override val isSquare = false
  override val isX = false
  override def centerBoard = Matrix(matrix, upperLeft + Position(1, 1), width - 2, height - 2)
}

abstract class LargeBoard(matrix: Vector[Vector[Int]],
  upperLeft: Position,
  val width: Int,
  val height: Int) extends BaseMatrix(matrix) with Matrix {
  def bottomRight = Position(upperLeft.x + width - 1, upperLeft.y + height - 1)
  def upperRight = Position(bottomRight.x, upperLeft.y)
  def bottomLeft = Position(upperLeft.x, bottomRight.y)

  def corners = Vector(upperLeft, upperRight, bottomLeft, bottomRight).map(valueAt _)

  def subBoard(newUpperLeft: Position, w: Int, h: Int) =
    Matrix(matrix, newUpperLeft, w, h)

  def immediateSubBoards: Vector[Matrix] =
    Vector(subBoard(upperLeft, width - 1, height),
      subBoard(upperLeft, width, height - 1),
      subBoard(upperLeft + Position(1, 0), width - 1, height),
      subBoard(upperLeft + Position(0, 1), width, height - 1))

  def legalSubBoards = immediateSubBoards.filter(_.isValid)

  def traverse: Stream[Matrix] = {
    def from(initial: Stream[Matrix], explored: Set[Matrix]): Stream[Matrix] = initial match { 
      case m #:: xs => {
        val subs = m.subBoardsSkipVisited(explored)
        // println(s"${m} => ${subs}")
        m #:: from(xs #::: subs.toStream, explored ++ subs)
      }
      case _ => Stream.empty
    }
    from(Stream(this), Set())
  }
}

object Matrix {
  implicit class AnyWithSideEffects[T](val v: T) extends AnyVal {
    def ~(f: T => Unit): T = {
      f(v)
      v
    }
  }
  
  def apply(board: Vector[Vector[Int]], start: Position, width: Int, height: Int): Matrix =
    (if (width == 0 || height == 0) EmptyMatrix
    else if (width == 1 && height == 1) SingleElementMatrix(board, start)
    else if (width == height) SquareMatrix(board, start, width)
    else NonSquareBoard(board, start, width, height))

  def apply(board: Seq[Seq[Int]]): Matrix = {
    val v = board.map(_.toVector).toVector
    val width = v.headOption.map(_.length).getOrElse(0)
    val height = if (width == 0) 0 else v.length
    Matrix(board = v, start = Position(0, 0), width = width, height = height)
  }
}
