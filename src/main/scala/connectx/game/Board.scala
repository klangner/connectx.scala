package connectx.game

import scala.math.{max, min}


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


  def mkString(): String = 
    val sep = "-" * (2 * width + 1)
    val rows = (height-1 to 0 by -1).map { row =>
      0.until(width).foldLeft("") {(acc, col) =>
          val cell = getStone(col, row) match 
              case CellType.Stone(StoneColor.Black) => "|x"
              case CellType.Stone(StoneColor.White) => "|o"
              case _ => "| "
          acc + cell
      } + "|"
    }
    val index = " " + 1.to(width).mkString(" ")
    val lines = Seq(sep) ++ rows ++ Seq(sep, index)
    lines.mkString("\n")


  // Convert 2D coords into array index 
  private def coords2idx(col: Int, row: Int): Int = 
    row * width + col
