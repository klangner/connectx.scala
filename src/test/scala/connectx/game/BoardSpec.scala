import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.game.Board
import connectx.game.StoneColor
import connectx.game.CellType


class BoardSpec extends AnyWordSpec  with Matchers {

    "Board" should {
      "start empty" in {
        val board = Board(7, 6)
        board.getStone(3, 0) shouldBe CellType.NoStone
      }
      
      "add stone" in {
        val board = Board(7, 6)
        board.putStone(3, StoneColor.Black)
        board.getStone(3, 0) shouldBe CellType.Stone(StoneColor.Black)
      }
      
      "add stone a top another stone" in {
        val board = Board(7, 6)
        board.putStone(3, StoneColor.Black)
        board.putStone(3, StoneColor.White)
        board.getStone(3, 1) shouldBe CellType.Stone(StoneColor.White)
        board.getStone(3, 0) shouldBe CellType.Stone(StoneColor.Black)
      }

      "serialize to/from string" in {
        val boardStr = 
          """|---------------
             || | | | | | | |
             || | | | | | | |
             || |x|o| | |x| |
             ||x|o|o|x| |o| |
             ||o|o|o|x|x|o|x|
             ||x|x|x|x|o|o|x|
             |---------------
             | 1 2 3 4 5 6 7""".stripMargin
        val board = Board.fromString(boardStr).get
        board.mkString() shouldBe boardStr
      }
    }
}