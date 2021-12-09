package connectx.app

import scala.io.StdIn.readLine

import connectx.game.{Board}
import connectx.Console
import connectx.game.StoneColor
import scala.annotation.tailrec

object HumanVsHuman:
  def main(args: Array[String]): Unit = 
    val board = Board(7, 6)

    play(board, StoneColor.Black)

  @tailrec
  def play(board: Board, stone: StoneColor): Unit = 
    Console.printBoard(board)
    if (stone == StoneColor.Black)
      println("black move: ")
    else
      println("white move: ")
    val col = safeInt(readLine()).getOrElse(0) - 1
    if (col < 0) return
    board.putStone(col, stone)
    play(board, if(stone == StoneColor.Black) StoneColor.White else StoneColor.Black)

  
  def safeInt(str: String): Option[Int] =
    try
      Some(str.toInt)
    catch 
      case e: Exception => None
  


