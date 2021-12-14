package connectx.agent

import connectx.game.Board


enum Action:
  case PutStone(col: Int)
  case Pass


trait Agent:
  def makeMove(board: Board): Action

  
