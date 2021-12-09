package connectx.game


enum StoneColor: 
  case Black, White


enum CellType:
  case Stone(c: StoneColor)
  case NoStone


case class Board(width: Int, height: Int):

  private val cells: Array[CellType] = Array.fill(width*height){CellType.NoStone}

  // Add new stone at given column. 
  // Return tru if stone was added or false otherwise
  def putStone(col: Int, stone: StoneColor): Boolean =
    if (col >= width) return false
    val emptyRow = 0.until(height)
      .map(r => coords2idx(col, r))
      .filter(i => cells(i) == CellType.NoStone)
      .headOption

    emptyRow match
      case Some(i) => 
        cells(i) = CellType.Stone(stone)
        true
      case _ => false


  // Get stone at given position
  def getStone(col: Int, row: Int): CellType =
    val idx = coords2idx(col, row)
    if (idx < cells.length) cells(idx) else CellType.NoStone


  // Convert 2D coords into array index 
  private def coords2idx(col: Int, row: Int): Int = 
    row * width + col