package connectx.app

import scala.annotation.tailrec

import connectx.game.{Board, StoneColor, GameResult}
import connectx.game.VictoryChecker
import connectx.agent.{Agent, Action, HumanPlayer, RandomAgent}


object PlayGame:

  def main(args: Array[String]): Unit = 
    println(args.mkString)
    runGame(args(0), args(1))


  def runGame(bot1: String, bot2: String): Unit = 
    val board = Board(7, 6)
    val blackPlayer = initAgent(bot1, StoneColor.Black)
    val whitePlayer = initAgent(bot2, StoneColor.White)
    val result = play(board, blackPlayer, whitePlayer, StoneColor.Black, false)
    println("")
    result match 
      case GameResult.BlackWon => println("Black won!")
      case GameResult.WhiteWon => println("White won!")
      case _ => println("Draw")

    println("")


  def initAgent(name: String, color: StoneColor): Agent = 
    name match
      case "random" => RandomAgent(color)
      case _        => HumanPlayer(color)


  @tailrec
  def play(board: Board, blackPlayer: Agent, whitePlayer: Agent, stone: StoneColor, lastPass: Boolean): GameResult = 
    println(board.mkString())
    println(s"$stone move: ")
    val action = if (stone == StoneColor.Black) blackPlayer.makeMove(board) else whitePlayer.makeMove(board)
    val isPass = action match
      case Action.Pass => true
      case Action.PutStone(col) => !board.putStone(col, stone)
    if(lastPass && isPass) return GameResult.Draw
    if(VictoryChecker.hasWon(board, stone))
      println(board.mkString())
      return if(stone == StoneColor.Black) GameResult.BlackWon else GameResult.WhiteWon
    val nextColor = if(stone == StoneColor.Black) StoneColor.White else StoneColor.Black
    play(board, blackPlayer, whitePlayer, nextColor, isPass)
