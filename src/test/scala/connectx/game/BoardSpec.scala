import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.game.Board
import connectx.game.StoneColor
import connectx.game.CellType


class BoardSpec extends AnyWordSpec  with Matchers{

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
    }
}