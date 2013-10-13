package puzzlers.sudoku

class Matrix(grid: Vector[Vector[Int]]) {
  def rows: Vector[Vector[Int]] = grid
  def columns: Vector[Vector[Int]] = grid.transpose
  def regions(count: Int): Vector[Matrix] = {
    val grouping = math.sqrt(count).intValue

    grid.grouped(grouping).flatMap { grouped =>
      grouped.transpose.grouped(grouping).map { flipped =>
        new Matrix(flipped.transpose)
      }
    }.toVector
  }

  def toSet = rows.foldLeft(Set[Int]())(_ ++ _)
  def length = grid.foldLeft(0) {
    _ + _.length
  }

  private val target = (1 to 9).sum
  def check = rows.toSet.size == 9 &&
    columns.toSet.size == 9 &&
    regions(9).toSet.size == 9 &&
    rows.forall(_.sum == target) &&
    columns.forall(_.sum == target) &&
    regions(9).forall(_.toSet.sum == target)

  override def toString = (for (rows <- grid) yield rows.mkString(" ")).mkString("\n")
  
  override def equals(other: Any) = other match { 
    case m: Matrix => m.rows == this.rows
    case _ => false
  }
}
