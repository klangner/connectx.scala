package connectx.game


enum StoneColor: 
  case Black, White


enum CellType:
  case Stone(c: StoneColor)
  case NoStone

enum GameResult:
  case Draw, BlackWon, WhiteWon


case class Board(width: Int, height: Int):

  private val cells: Array[CellType] = Array.fill(width*height){CellType.NoStone}

  // Add new stone at given column. 
  // Return tru if stone was added or false otherwise
  def putStone(col: Int, stone: StoneColor): Boolean =
    if (col >= width || col < 0) return false
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


object RuleChecker:
  // Find 4 stone in a row
  def hasWon(board: Board, color: StoneColor): Boolean = 
    val rowPattern = 0.until(board.height).foldLeft(false) { (acc, row) => 
      acc || checkRowStones(board, row, color)
    }
    val colPattern = 0.until(board.width).foldLeft(false) { (acc, col) => 
      acc || checkColumnStones(board, col, color)
    }
    rowPattern || colPattern

  // Check if row contains 4 stones in a row
  def checkRowStones(board: Board, row: Int, color: StoneColor): Boolean =
    val maxGroupSize = 0.until(board.width).foldLeft(0) { (acc, col) =>
      if(board.getStone(col, row) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4

  // Check if column contains 4 stones in a row
  def checkColumnStones(board: Board, col: Int, color: StoneColor): Boolean =
    val maxGroupSize = 0.until(board.height).foldLeft(0) { (acc, row) =>
      if(board.getStone(col, row) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4
