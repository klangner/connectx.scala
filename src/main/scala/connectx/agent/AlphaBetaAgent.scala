package connectx.agent

import connectx.game.{Board, StoneColor, VictoryChecker}


// gent baed on Alpha Beta (min-max) algorithm
class AlphaBetaAgent(color: StoneColor, searchDepth: Int) extends Agent:
  
  private val rnd = new scala.util.Random

  override def toString: String = s"Alpha_$searchDepth"

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
    val col = searchMove(board, color, 1)._1
    Action.PutStone(col)

  
  // Verify move and return (column, score) for this move where:
  // 1  - Winning move
  // 0  - Draw
  // -1 - Loosing move
  def searchMove(board: Board, stone: StoneColor, depth: Int): (Int, Int) = 
    // Look for winning move
    for(col <- 0.until(board.width))
      val b = board.putStone(col, stone) 
      if (VictoryChecker.hasWon(b, stone))
        return (col, 1)

    // If this is max allowed depth then return random column with draw as a result
    if(depth == searchDepth)
      return (rnd.nextInt(board.width), 0)
      
    // Look down the tree
    val moves = 0.until(board.width).map { col =>
      val b = board.putStone(col, stone)
      val m = searchMove(b, stone.other, depth+1)
      (col, -m._2)
    }
    val maxScore = moves.map(_._2).max
    val bestMoves = moves.filter(_._2 == maxScore)
    bestMoves(rnd.nextInt(bestMoves.length))