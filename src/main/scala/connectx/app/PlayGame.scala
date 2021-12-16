package connectx.app

import scala.annotation.tailrec

import connectx.game.{Board, StoneColor, GameResult}
import connectx.game.VictoryChecker
import connectx.agent.{Agent, Action, AlphaBetaAgent, HumanPlayer, MCAgent, RandomAgent}


object PlayGame:

  def main(args: Array[String]): Unit = 
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
      case "alpha" =>  AlphaBetaAgent(color, 4)
      case "MC" =>     MCAgent(color, 4)
      case _        => HumanPlayer(color)


  @tailrec
  def play(board: Board, blackPlayer: Agent, whitePlayer: Agent, stone: StoneColor, lastPass: Boolean): GameResult = 
    println(board.toString())
    println(s"$stone move: ")
    val player = if (stone == StoneColor.Black) blackPlayer else whitePlayer 
    val action = makeMove(board, player) 
    val (newBoard, isPass) = action match
      case Action.Pass => (board, true)
      case Action.PutStone(col) => (board.putStone(col, stone), false)
    if(lastPass && isPass) return GameResult.Draw
    if(VictoryChecker.hasWon(newBoard, stone))
      println(newBoard.toString())
      return GameResult.fromStone(stone) 
    play(newBoard, blackPlayer, whitePlayer, stone.other, isPass)


  // Convert illegal action into pass
  def makeMove(board: Board, agent: Agent): Action = 
    val action = agent.makeMove(board)
    action match
      case Action.Pass => action
      case Action.PutStone(col) => 
        if (board.canPutStone(col)) action
        else Action.Pass
