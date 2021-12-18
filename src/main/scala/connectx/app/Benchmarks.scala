package connectx.app

import scala.annotation.tailrec

import connectx.game.{Board, StoneColor, GameResult}
import connectx.game.VictoryChecker
import connectx.agent.{Agent, Action, AlphaBetaAgent, HumanPlayer, MCAgent, RandomAgent}


object Benchmarks:

  def main(args: Array[String]): Unit = 
    val numSims = 1_000
    val board = Board(7, 6)
    val agent = AlphaBetaAgent(StoneColor.Black, 4)
    // val agent = MCAgent(StoneColor.Black, 1_000)
    val startTime = System.currentTimeMillis()
    val actions = 0.until(numSims).map { _ =>
      agent.makeMove(board)
    }
    val totalTime = (System.currentTimeMillis() - startTime) / 1000
    val passes = actions.filter(_ == Action.Pass).length
    println(s"Run $numSims in $totalTime seconds. Passes: $passes")

