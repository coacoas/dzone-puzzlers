package puzzlers.sudoku

class Matrix(grid: Vector[Vector[Int]]) {
  def rows: Vector[Vector[Int]] = grid
  def columns: Vector[Vector[Int]] = grid.transpose
  def regions(count: Int): Vector[Matrix] = {
    val grouping = math.sqrt(count).intValue
    
    val rs = for { 
      split <- grid.grouped(grouping)
      splitT = split.transpose
      split2 <- splitT.grouped(grouping)
    } yield (new Matrix(split2.transpose))
    
    rs.toVector
  }

  def toSet = rows.foldLeft(Set[Int]())(_ ++ _)
  def length = grid.foldLeft(0) {
    _ + _.length
  }

  def hasSudokuProperties(s: Set[Int]) = 
    s.size == 9 && s.sum == target
  
  private val target = (1 to 9).sum
  def check = {
    val rowSets = rows.map(_.toSet)
    val colSets = columns.map(_.toSet)
    val regSets = regions(9).map(_.toSet)
    
    (rowSets ++ colSets ++ regSets).map(hasSudokuProperties).reduce(_ && _)
  }
  
  override def toString = (for (rows <- grid) yield rows.mkString(" ")).mkString("\n")
  
  override def equals(other: Any) = other match { 
    case m: Matrix => m.rows == this.rows
    case _ => false
  }
}
