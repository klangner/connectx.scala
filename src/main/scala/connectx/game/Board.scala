package connectx.game

import scala.math.{max, min}


enum StoneColor: 
  case Black, White


enum CellType:
  case Stone(c: StoneColor)
  case NoStone

enum GameResult:
  case Draw, BlackWon, WhiteWon


object Board:
  def fromString(str: String): Option[Board] = 
    val lines = str.split("\n")
    if (lines.length < 4) return None
    val rowLines = lines.slice(1, lines.length-2).reverse
    val rows = rowLines.length
    val cols = (rowLines(0).length - 1) / 2
    if (cols < 1) return None
    val board = rowLines.foldLeft(Board(cols, rows)) {(b, row) =>
      0.until(cols).foldLeft(b){ (b2, col) =>
        row(2*col+1) match 
          case 'x' => b2.putStone(col, StoneColor.Black)
          case 'o' => b2.putStone(col, StoneColor.White)
          case _   => b2
      }
    }

    Some(board)


class Board(val width: Int, val height: Int, private val cells: Vector[CellType]):

  def this(width: Int, height: Int) = 
    this(width, height, Vector.fill(width*height)(CellType.NoStone))


  def canPutStone(col: Int): Boolean = 
    if (col >= width || col < 0) return false
    getStone(col, height-1) == CellType.NoStone


  // Add new stone at given column. 
  // Return new Board if stone was added or false otherwise
  def putStone(col: Int, stone: StoneColor): Board =
    if (col >= width || col < 0) return this
    val emptyRow = 0.until(height)
      .map(r => coords2idx(col, r))
      .filter(i => cells(i) == CellType.NoStone)
      .headOption

    emptyRow match
      case Some(i) => 
        val newCells = cells.updated(i, CellType.Stone(stone))
        Board(width, height, newCells)
      case _ => this


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
