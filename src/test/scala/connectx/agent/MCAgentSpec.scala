import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.agent.MCAgent
import connectx.agent.MCAgent.{Node, State}
import connectx.game.{Board, CellType, GameResult, StoneColor}


class MCAgentSpec extends AnyWordSpec  with Matchers {

  "MCAgent" should {

    "Select itself" in {
      val boardStr = 
        """|---------------
           || |x| | | | | |
           || |x| | | | | |
           || |x|o| | |x| |
           ||x|o|o|x| |o| |
           ||o|o|o|x|x|o|x|
           ||x|x|x|x|o|o|x|
           |---------------
           | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      val state = State(board, StoneColor.White)
      val rootNode = Node(None, state)

      rootNode.select() shouldBe rootNode
    }
    
    "Expand root node" in {
      val boardStr = 
        """|---------------
           || |x| | | | | |
           || |x| | | | | |
           || |x|o| | |x| |
           ||x|o|o|x| |o| |
           ||o|o|o|x|x|o|x|
           ||x|x|o|x|o|o|x|
           |---------------
           | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      val state = State(board, StoneColor.White)
      val rootNode = Node(None, state)
      val child = rootNode.expand()

      child should not be rootNode
    }
    
    "Do not expand winning node" in {
      val boardStr = 
        """|---------------
           || |x| | | | | |
           || |x| | | | | |
           || |x|o| | |x| |
           ||x|o|o|x| |o| |
           ||o|o|o|x|x|o|x|
           ||x|x|x|x|o|o|x|
           |---------------
           | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      val state = State(board, StoneColor.White)
      val rootNode = Node(None, state)
      rootNode.expand() shouldBe rootNode
    }
    
    "Propagate score" in {
      val boardStr = 
        """|---------------
           || |x| | | | | |
           || |x| | | | | |
           || |x|o| | |x| |
           ||x|o|o|x| |o| |
           ||o|o|o|x|x|o|x|
           ||x|o|x|x|o|o|x|
           |---------------
           | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      val state = State(board, StoneColor.White)
      val rootNode = Node(None, state)
      val child = rootNode.expand()
      val result = child.simulate()
      child.backpropagate(1.0)
      rootNode.ucb1(1) shouldBe 1.0
      child.ucb1(1) shouldBe 1.0
    }
  }
}