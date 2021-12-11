package connectx.app

import scala.io.StdIn.readLine

import connectx.game.{Board, StoneColor, GameResult}
import scala.annotation.tailrec

object HumanVsHuman:
  def main(args: Array[String]): Unit = 
    val board = Board(7, 6)
    val result = play(board, StoneColor.Black, false)
    println("")
    result match 
      case GameResult.BlackWon => println("Black won!")
      case GameResult.WhiteWon => println("White won!")
      case _ => println("Draw")

    println("")


  @tailrec
  def play(board: Board, stone: StoneColor, lastPass: Boolean): GameResult = 
    println(board.mkString())
    if (stone == StoneColor.Black)
      print("black move: ")
    else
      print("white move: ")
    val col = safeInt(readLine()).getOrElse(0) - 1
    val isPass = !board.putStone(col, stone)
    if(lastPass && isPass) return GameResult.Draw
    val nextColor = if(stone == StoneColor.Black) StoneColor.White else StoneColor.Black
    play(board, nextColor, isPass)

  
  def safeInt(str: String): Option[Int] =
    try
      Some(str.toInt)
    catch 
      case e: Exception => None
  


