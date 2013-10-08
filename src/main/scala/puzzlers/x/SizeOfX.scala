package puzzlers.x

case class Position(val x: Int, val y: Int) {
  def +(other: Position) = Position(x + other.x, y + other.y)
  def -(other: Position) = Position(x - other.x, y - other.y)
}

case class Board(val board: Vector[Vector[Int]], upperLeft: Position, width: Int, height: Int) {
  def bottomRight = Position(upperLeft.x + width - 1, upperLeft.y + height - 1)
  def upperRight = Position(bottomRight.x, upperLeft.y)
  def bottomLeft = Position(upperLeft.x, bottomRight.y)

  def corners = Vector(upperLeft, upperRight, bottomLeft, bottomRight).map(valueAt _)

  val isEmpty = width <= 0 || height <= 0
  val isSquare = width == height
  def isX: Boolean =
    isSquare &&
      (!isEmpty) &&
      ((width == 1 && (valueAt(upperLeft) == 1)) ||
        (corners.forall(_ == 1)) && centerBoard.isX)

  def centerBoard = subBoard(upperLeft + Position(1, 1), width - 2, height - 2)

  def valueAt(p: Position) = board(p.x)(p.y)
  def subBoard(newUpperLeft: Position, w: Int, h: Int) = new Board(board, newUpperLeft, w, h)

  def immediateSubBoards: Stream[Board] = (subBoard(upperLeft, width - 1, height) #::
    subBoard(upperLeft, width, height - 1) #::
    subBoard(upperLeft + Position(1, 0), width - 1, height) #::
    subBoard(upperLeft + Position(0, 1), width, height - 1) #:: Stream.empty[Board]).filter(b => !b.isEmpty)

  def subBoardsSkipVisited(visited: Set[Board]): Stream[Board] = immediateSubBoards.filter(!visited.contains(_))

  lazy val traverse: Stream[Board] = {
    def from(initial: Stream[Board], explored: Set[Board]): Stream[Board] = {
      val (nextInitial, nextExplored) = initial.foldLeft((Stream.empty[Board], explored)) {
        case ((acc, exploredPlus), e) =>
          println(exploredPlus)
          println(e)
          println("---------------------------")
          (acc #::: e.subBoardsSkipVisited(exploredPlus), exploredPlus + e)
      }
      initial #::: from(nextInitial, nextExplored)
    }
    from(Stream(this), Set())
  }
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