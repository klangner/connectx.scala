package connectx.agent

import connectx.game.Board
import connectx.game.StoneColor


class RandomAgent(color: StoneColor) extends Agent:
  
    private val rnd = new scala.util.Random

    override def makeMove(board: Board): Action = 
        Action.PutStone(rnd.nextInt(board.width))
