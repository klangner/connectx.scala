package connectx.app

import connectx.game.{Board}
import connectx.Console
import connectx.game.StoneColor

object HumanVsHuman:
  def main(args: Array[String]): Unit = 
    val board = Board(7, 6)
    board.putStone(3, StoneColor.Black)
    board.putStone(3, StoneColor.White)
    board.putStone(3, StoneColor.Black)
    board.putStone(2, StoneColor.White)
    Console.printBoard(board)

