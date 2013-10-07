package puzzlers.x

case class Position(val x: Int, val y: Int)

case class Board(val board: Vector[Vector[Int]], upperLeft: Position, width: Int, height: Int) {
  def bottomRight = Position(upperLeft.x + width - 1, upperLeft.y + height - 1)
  def upperRight = Position(bottomRight.x, upperLeft.y)
  def bottomLeft = Position(upperLeft.x, bottomRight.y)

  def corners = Vector(upperLeft, upperRight, bottomLeft, bottomRight).map(valueAt _)

  val isValid = width >= 0 && height >= 0
  val isSquare = width == height
  def isX: Boolean = isValid && isSquare &&
    (width == 0 ||
      (width == 1 && (valueAt(upperLeft) == 1)) ||
      (corners.forall(_ == 1)) &&
      subBoard(Position(upperLeft.x + 1, upperLeft.y + 1), width - 2, height - 2).isX)

  def valueAt(p: Position) = board(p.x)(p.y)
  def subBoard(newUpperLeft: Position, w: Int, h: Int) = new Board(board, newUpperLeft, w, h)
}

object Board {
  def apply(board: Seq[Seq[Int]]) = {
    val v = board.map(_.toVector).toVector
    val width = v.headOption.map(_.length).getOrElse(0)
    val height = if (width == 0) 0 else v.length
    new Board(board = v,
      upperLeft = Position(0, 0),
      width = width,
      height = height)
  }
}

class SizeOfX {

}