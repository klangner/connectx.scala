import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.game.Board
import connectx.game.StoneColor._
import connectx.game.CellType
import connectx.game.VictoryChecker


class VictoryCheckerSpec extends AnyWordSpec  with Matchers {

  "RuleChecker" should {
    "find 4 in a row" in {
      val board = Board(7, 6)
      board.putStone(0, Black)
      board.putStone(1, White)
      board.putStone(2, Black)
      board.putStone(3, Black)
      board.putStone(4, Black)
      board.putStone(5, Black)
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
      
    "find 4 in a column" in {
      val board = Board(7, 6)
      board.putStone(3, Black)
      board.putStone(3, White)
      board.putStone(3, Black)
      board.putStone(3, Black)
      board.putStone(3, Black)
      board.putStone(3, Black)
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
      
    "find 4 left to right cross first" in {
      val boardStr = 
        """|---------------
            || | | |x| | | |
            || | |x|x| | | |
            || |x|o|o| |x| |
            ||x|o|o|x| |o| |
            ||o|o|o|x|x|o|x|
            ||x|o|x|x|o|o|x|
            |---------------
            | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
      
    "find 4 left to right cross last" in {
      val boardStr = 
        """|---------------
            || | | |x| | | |
            || | |x|x| | | |
            || |x|o|o| |x|x|
            ||o|o|o|x| |x|o|
            ||o|o|o|x|x|o|x|
            ||x|o|x|x|o|o|x|
            |---------------
            | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
      
    "find 4 rigth to left cross first" in {
      val boardStr = 
        """|---------------
            || | | | | | | |
            ||x| | |o| | | |
            ||x|x|x|o| |x| |
            ||x|x|o|x| |o| |
            ||o|o|x|o|x|o|o|
            ||x|o|x|x|o|o|x|
            |---------------
            | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
      
    "find 4 rigth to left cross last" in {
      val boardStr = 
        """|---------------
            || | | |x| | | |
            ||x| | |x|x| | |
            ||o|x|x|o|o|x| |
            ||x|x|o|x|x|o|x|
            ||o|o|x|o|x|o|o|
            ||x|o|x|x|o|o|x|
            |---------------
            | 1 2 3 4 5 6 7""".stripMargin
      val board = Board.fromString(boardStr).get
      VictoryChecker.hasWon(board, White) shouldBe false
      VictoryChecker.hasWon(board, Black) shouldBe true
    }
  }
}