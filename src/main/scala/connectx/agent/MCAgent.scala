package connectx.agent

import connectx.game.{Board, GameResult, StoneColor}
import connectx.game.VictoryChecker


object MCAgent:

  private val rnd = new scala.util.Random

  case class State(board: Board, color: StoneColor)

  case class Node(parent: Option[Node], state: State):
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
    def expand(): Option[Node] = 
      if (VictoryChecker.hasWon(state.board, state.color.other))
        return None
      val board = state.board
      children = board.validMoves.map { col =>
        val b = board.putStone(col, state.color)
        Node(Some(this), State(b, state.color.other))
      }
      if(children.length > 0) Some(children(rnd.nextInt(children.length)))
      else None


    // Simulate rollout
    def simulate(): GameResult = 
      GameResult.Draw


    def propagate(score: Float): Unit = 
      totalScore += score
      visitCount += 1
      parent.map(_.propagate(score))

    
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
      // Selection
      // val selectNode = rootNode.select()
      // Expansion
      // val simNode = selectNode.expand()
      // Simulation
      // val score = simNode.simulate()
      // Backpropagation
      // simNode.backpropagate(score)
    }
    // val bestNode = rootNode.bestNode()

    Action.PutStone(rnd.nextInt(board.width))

  