package connectx

import connectx.game.{Board, CellType, StoneColor}


object Console:
    def printBoard(board: Board): Unit = 
        val line = "-" * (2 * board.width + 1)
        println(line)
        for (row <- board.height-1 to 0 by -1)
            val rowLine = 0.until(board.width).foldLeft("") {(acc, col) =>
                val cell = board.getStone(col, row) match 
                    case CellType.Stone(StoneColor.Black) => "|x"
                    case CellType.Stone(StoneColor.White) => "|o"
                    case _ => "| "
                acc + cell
            } + "|"
            println(rowLine)
        println(line)
