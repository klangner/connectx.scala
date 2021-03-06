package connectx.app

import scala.annotation.tailrec

import connectx.game.{Board, StoneColor, GameResult}
import connectx.game.VictoryChecker
import connectx.agent.{Agent, Action, AlphaBetaAgent, HumanPlayer, MCAgent, RandomAgent}


object EvaluatePlayers:

  def main(args: Array[String]): Unit = 
    val numGames = 1_000
    val player1 = args(0)
    val player2 = args(1)
    val startTime = System.currentTimeMillis
    // Player1 == black
    val results1 = 0.until(numGames/2).map(_ => runGame(player1, player2))
    // Player2 == black
    val results2 = 0.until(numGames/2).map(_ => runGame(player2, player1))
    val player1Wins = 
      results1.filter(_ == GameResult.BlackWon).length + results2.filter(_ == GameResult.WhiteWon).length
    val player2Wins = 
      results1.filter(_ == GameResult.WhiteWon).length + results2.filter(_ == GameResult.BlackWon).length
    val draws = numGames - (player1Wins + player2Wins)
    val totalTime = (System.currentTimeMillis - startTime) / 1000
    println(s"Played $numGames in $totalTime seconds")
    println(s"$player1 wins: $player1Wins (${100*player1Wins/numGames}%)")
    println(s"$player2 wins: $player2Wins (${100*player2Wins/numGames}%)")
    println(s"Draw: $draws (${100*draws/numGames}%)")


  def runGame(bot1: String, bot2: String): GameResult = 
    val board = Board(7, 6)
    val blackPlayer = PlayGame.initAgent(bot1, StoneColor.Black)
    val whitePlayer = PlayGame.initAgent(bot2, StoneColor.White)
    play(board, blackPlayer, whitePlayer, StoneColor.Black, false)


  @tailrec
  def play(board: Board, blackPlayer: Agent, whitePlayer: Agent, stone: StoneColor, lastPass: Boolean): GameResult = 
    val player = if (stone == StoneColor.Black) blackPlayer else whitePlayer 
    val action = makeMove(board, player) 
    val (newBoard, isPass) = action match
      case Action.Pass => (board, true)
      case Action.PutStone(col) => (board.putStone(col, stone), false)
    if(lastPass && isPass) return GameResult.Draw
    if(VictoryChecker.hasWon(newBoard, stone))
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
