package connectx.agent

import scala.io.StdIn.readLine

import connectx.game.Board
import connectx.game.StoneColor


// Human play game via console
class HumanPlayer(color: StoneColor) extends Agent:
  override def makeMove(board: Board): Action = 
    val col = safeInt(readLine()).getOrElse(0) - 1
    if (col >= 0) Action.PutStone(col) else Action.Pass

    
  def safeInt(str: String): Option[Int] =
    try
      Some(str.toInt)
    catch 
      case e: Exception => None
  

  override def toString(): String = "Human"