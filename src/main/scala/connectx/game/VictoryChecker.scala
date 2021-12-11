package connectx.game

import scala.math.{max, min}


object VictoryChecker:

  // Find 4 stone in a row
  def hasWon(board: Board, color: StoneColor): Boolean = 
    // Check rows
    for(row <- 0.until(board.height))
      if(checkRow(board, row, color))
        return true
    
    // Check columns
    for (col <- 0.until(board.width))
      if (checkColumn(board, col, color))
        return true
    
    // Check left to right
    val maxLines = board.width + board.height - 2*4 + 1
    for (pos <- 0.until(maxLines))
      if (checkLeftRight(board, pos, color))
        return true
    
    // Check left to right
    for (pos <- 0.until(maxLines))
      if (checkRightLeft(board, pos, color))
        return true

    false


  // Check if row contains 4 stones in a row
  def checkRow(board: Board, row: Int, color: StoneColor): Boolean =
    val maxGroupSize = 0.until(board.width).foldLeft(0) { (acc, col) =>
      if(board.getStone(col, row) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4


  // Check if column contains 4 stones in a row
  def checkColumn(board: Board, col: Int, color: StoneColor): Boolean =
    val maxGroupSize = 0.until(board.height).foldLeft(0) { (acc, row) =>
      if(board.getStone(col, row) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4


  // Check accross left to right
  def checkLeftRight(board: Board, pos: Int, color: StoneColor): Boolean =
    val startCol = max(pos - board.height + 4, 0)
    val startRow = max(board.height - pos - 4, 0)
    val count = min(board.width, board.height)
    val maxGroupSize = 0.until(count).foldLeft(0) { (acc, i) =>
      if(board.getStone(startCol+i, startRow+i) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4


  // Check accross left to right
  def checkRightLeft(board: Board, pos: Int, color: StoneColor): Boolean =
    val startCol = min(pos + 3, board.width-1)
    val startRow = max(pos + 3 - (board.width-1), 0)
    val count = min(board.width, board.height)
    val maxGroupSize = 0.until(count).foldLeft(0) { (acc, i) =>
      if(board.getStone(startCol-i, startRow+i) == CellType.Stone(color)) acc + 1
      else if (acc >= 4) acc
      else 0
    }
    maxGroupSize >= 4