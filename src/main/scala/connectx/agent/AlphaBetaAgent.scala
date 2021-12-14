package connectx.agent

import connectx.game.{Board, StoneColor, VictoryChecker}


// gent baed on Alpha Beta (min-max) algorithm
class AlphaBetaAgent(color: StoneColor) extends Agent:
  
  private val rnd = new scala.util.Random

  // In first step select winning move if possible
  override def makeMove(board: Board): Action = 
    val winningCol: Option[Int] = 0.until(board.width).foldLeft(Option.empty) { (a, col) =>
      a match
        case None => 
          val b = board.putStone(col, color)
          if (VictoryChecker.hasWon(b, color)) Some(col)
          else None
        case _ => a
    }
    val col = winningCol.getOrElse(rnd.nextInt(board.width))
    Action.PutStone(col)
