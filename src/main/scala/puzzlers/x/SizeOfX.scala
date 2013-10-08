package puzzlers.x

case class Position(val x: Int, val y: Int) {
  def +(other: Position) = Position(x + other.x, y + other.y)
  def -(other: Position) = Position(x - other.x, y - other.y)
}

sealed trait Board { 
  def isSquare: Boolean
  def isX: Boolean
  def traverse: Stream[Board]
  def centerBoard: Board
  def immediateSubBoards: Stream[Board]
  def subBoardsSkipVisited(visited: Set[Board]) = immediateSubBoards.filterNot(visited)
}

case object EmptyBoard extends Board { 
  override val isSquare = true
  override val isX = false
  override val centerBoard = EmptyBoard
  override val traverse = Stream.empty
  override val immediateSubBoards = Stream.empty
}

case class OneSquareBoard(matrix: Vector[Vector[Int]], position: Position) extends Board { 
  val value = matrix(position.x)(position.y)
  override val isSquare = true
  override val isX = value == 1
  override val centerBoard = EmptyBoard
  override val traverse = Stream.empty
  override val immediateSubBoards = Stream.empty
}

case class SquareBoard(matrix: Vector[Vector[Int]], upperLeft: Position, size: Int) extends 
    LargeBoard(matrix, upperLeft, size, size){ 
  override val isSquare = true
  override def centerBoard = Board(matrix, upperLeft + Position(1, 1), size - 2, size - 2)
  override def isX = corners.forall(_ == 1) && centerBoard.isX
}

case class NonSquareBoard(matrix: Vector[Vector[Int]], upperLeft: Position, width: Int, height: Int) extends
LargeBoard(matrix, upperLeft, width, height) {
  override val isSquare = false
  override val isX = false
  override def centerBoard = Board(matrix, upperLeft + Position(1, 1), width - 2, height - 2)
}

abstract class LargeBoard(matrix: Vector[Vector[Int]], 
    upperLeft: Position, 
    width: Int, 
    height: Int) extends Board {
  def bottomRight = Position(upperLeft.x + width - 1, upperLeft.y + height - 1)
  def upperRight = Position(bottomRight.x, upperLeft.y)
  def bottomLeft = Position(upperLeft.x, bottomRight.y)

  def corners = Vector(upperLeft, upperRight, bottomLeft, bottomRight).map(valueAt _)

  def valueAt(p: Position): Option[Int] = try { 
    Some(matrix(p.x)(p.y))
  } catch { 
    case _: Exception => None
  }
  
  def subBoard(newUpperLeft: Position, w: Int, h: Int) = Board(matrix, newUpperLeft, w, h)

  def immediateSubBoards: Stream[Board] = (subBoard(upperLeft, width - 1, height) #::
    subBoard(upperLeft, width, height - 1) #::
    subBoard(upperLeft + Position(1, 0), width - 1, height) #::
    subBoard(upperLeft + Position(0, 1), width, height - 1) #:: Stream.empty[Board])

  def traverse: Stream[Board] = {
    def from(initial: Stream[Board], explored: Set[Board]): Stream[Board] = {
      val (nextInitial, nextExplored) = initial.foldLeft((Stream.empty[Board], explored)) {
        case ((acc, exploredPlus), e) =>
          println(exploredPlus)
          println(e)
          println("---------------------------")
          (acc #::: (e.immediateSubBoards.filterNot(exploredPlus)), exploredPlus + e)
      }
      initial #::: from(nextInitial, nextExplored)
    }
    from(Stream(this), Set())
  }
}

object Board {
  def apply(board: Vector[Vector[Int]], start: Position, width: Int, height: Int): Board = 
    if (width == 0 || height == 0) EmptyBoard
    else if (width == 1 && height == 1) OneSquareBoard(board, start)
    else if (width == height) SquareBoard(board, start, width)
    else NonSquareBoard(board, start, width, height)
    
  def apply(board: Seq[Seq[Int]]): Board = {
    val v = board.map(_.toVector).toVector
    val width = v.headOption.map(_.length).getOrElse(0)
    val height = if (width == 0) 0 else v.length
    Board(board = v,
      start = Position(0, 0),
      width = width,
      height = height)
  }
}