package connectx.agent

import connectx.game.{Board, GameResult, StoneColor}
import connectx.game.VictoryChecker
import scala.annotation.tailrec


object MCAgent:

  private val rnd = new scala.util.Random

  case class State(board: Board, color: StoneColor)

  case class Node(parent: Option[Node], state: State, action: Action = Action.Pass):
    private var children: Seq[Node] = Seq.empty
    private var visitCount: Int = 0
    private var totalScore: Float = 0

    // If we have children the select on with UCB1 function and then call select on it
    // Otherwise select itself
    def select(): Node = 
      if(children.length > 0)
        val scores = children.map(_.ucb1(visitCount))
        val argMax = scores.indices.maxBy(scores)
        children(argMax)
      else this


    // Expand state with all possible valid moves
    // Return random children or None is no children were created
    def expand(): Node = 
      if (VictoryChecker.hasWon(state.board, state.color.other))
        return  this
      val board = state.board
      children = board.validMoves.map { col =>
        val b = board.putStone(col, state.color)
        Node(Some(this), State(b, state.color.other), Action.PutStone(col))
      }
      if(children.length > 0) children(rnd.nextInt(children.length))
      else this


    // Simulate rollout
    def simulate(): GameResult = 
      simulate(state.board, state.color)


    // Helper simulate function
    @tailrec private def simulate(board: Board, color: StoneColor): GameResult = 
      if (VictoryChecker.hasWon(board, color.other))
        return GameResult.fromStone(color.other) 

      val validMoves = board.validMoves
      if (validMoves.length == 0)
        return GameResult.Draw

      val col = validMoves(rnd.nextInt(validMoves.length))
      val b = board.putStone(col, color)
      simulate(b, color.other)


    def backpropagate(score: Float): Unit = 
      totalScore += score
      visitCount += 1
      parent.map(_.backpropagate(score))

    
    def bestNode(): Node = 
      if (children.length == 0) 
        return this

      val scores = children.map(_.visitCount)
      val argMax = children.indices.maxBy(scores)
      children(argMax)

    
    // Calculate Upper Confidence Bound for trees to decide which node to select
    def ucb1(parentRollouts: Int): Double = 
      if (visitCount == 0) Double.MaxValue
      else totalScore/visitCount + 2.0 * math.sqrt(math.log(parentRollouts) / visitCount)


// Monte Carlo Tree Search based agent
class MCAgent(color: StoneColor, rolloutCount: Int) extends Agent:
  
  import  MCAgent._

  override def makeMove(board: Board): Action = 
    val rootNode = Node(None, State(board, color))
    for(_ <- 0.until(rolloutCount)) {
      val selectNode = rootNode.select()
      val node = selectNode.expand()
      val result = node.simulate()
      val score: Float = result match
        case GameResult.Draw => 0.0
        case GameResult.BlackWon => if(color == StoneColor.Black) 1.0 else -1.0
        case GameResult.WhiteWon => if(color == StoneColor.White) 1.0 else -1.0
      node.backpropagate(score)
    }
    val bestNode = rootNode.bestNode()
    bestNode.action

  