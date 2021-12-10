import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.game.Board
import connectx.game.StoneColor
import connectx.game.CellType
import connectx.game.RuleChecker


class RuleCheckerSpec extends AnyWordSpec  with Matchers{

    "RuleChecker" should {
      "find 4 in a row" in {
        val board = Board(7, 6)
        board.putStone(0, StoneColor.Black)
        board.putStone(1, StoneColor.White)
        board.putStone(2, StoneColor.Black)
        board.putStone(3, StoneColor.Black)
        board.putStone(4, StoneColor.Black)
        board.putStone(5, StoneColor.Black)
        RuleChecker.hasWon(board, StoneColor.White) shouldBe false
        RuleChecker.hasWon(board, StoneColor.Black) shouldBe true
      }
    }
}