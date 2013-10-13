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
  def immediateSubMatrices: Stream[Matrix]
  def legalImmediateSubMatrices: Stream[Matrix] = immediateSubMatrices.filter(_.isValid)
  def subMatricesSkipVisited(visited: Set[Matrix]): Stream[Matrix] = 
    legalImmediateSubMatrices.filterNot(visited)
  def largestX: Option[SquareMatrix] = traverse.collect({ 
    case s: SquareMatrix if s.size % 2 == 1 => s
  }).find(_.isX)
}

case object EmptyMatrix extends Matrix {
  override val isValid = false
  override val isSquare = true
  override val isX = false
  override val traverse = Stream.empty
  override val immediateSubMatrices = Stream.empty
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
  override val traverse = Stream.empty
  override val immediateSubMatrices = Stream.empty
}

case class SquareMatrix(matrix: Vector[Vector[Int]], upperLeft: Position, size: Int) extends LargeMatrix(matrix, upperLeft, size, size) {
  def bottomRight = Position(upperLeft.x + width - 1, upperLeft.y + height - 1)
  def upperRight = Position(bottomRight.x, upperLeft.y)
  def bottomLeft = Position(upperLeft.x, bottomRight.y)
  def corners = Vector(upperLeft, upperRight, bottomLeft, bottomRight).map(valueAt _)
  def centerBoard = Matrix(matrix, upperLeft + Position(1, 1), size - 2, size - 2)

  override def isValid = (upperLeft.y + size) < matrix.length
  override val isSquare = true
  override def isX = corners.forall(_ == Some(1)) && centerBoard.isX
}

case class NonSquareMatrix(matrix: Vector[Vector[Int]], upperLeft: Position, override val width: Int, override val height: Int) extends LargeMatrix(matrix, upperLeft, width, height) {
  override def isValid = (upperLeft.y + height) <= matrix.length && (upperLeft.x + width) <= matrix.head.length
  override val isSquare = false
  override val isX = false
}

abstract class LargeMatrix(matrix: Vector[Vector[Int]],
  upperLeft: Position,
  val width: Int,
  val height: Int) extends BaseMatrix(matrix) with Matrix {

  override def immediateSubMatrices =
    Matrix(matrix, upperLeft, width - 1, height) #:: 
      Matrix(matrix, upperLeft, width, height - 1) #:: 
      Matrix(matrix, upperLeft + Position(1, 0), width - 1, height) #:: 
      Matrix(matrix, upperLeft + Position(0, 1), width, height - 1) #:: Stream.empty

  def traverse: Stream[Matrix] = {
    def from(initial: Stream[Matrix], explored: Set[Matrix]): Stream[Matrix] = initial match { 
      case m #:: xs => {
        val subs = m.subMatricesSkipVisited(explored)
        m #:: from(xs #::: subs, explored ++ subs)
      }
      case _ => Stream.empty
    }
    from(Stream(this), Set())
  }
}

object Matrix {
  def apply(grid: Vector[Vector[Int]], start: Position, width: Int, height: Int): Matrix =
    (if (width == 0 || height == 0) EmptyMatrix
    else if (width == 1 && height == 1) SingleElementMatrix(grid, start)
    else if (width == height) SquareMatrix(grid, start, width)
    else NonSquareMatrix(grid, start, width, height))

  def apply(grid: Seq[Seq[Int]]): Matrix = {
    val v = grid.map(_.toVector).toVector
    val width = v.headOption.map(_.length).getOrElse(0)
    val height = if (width == 0) 0 else v.length
    Matrix(grid = v, start = Position(0, 0), width = width, height = height)
  }
}
