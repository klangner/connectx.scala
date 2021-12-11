import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import connectx.game.Board
import connectx.game.StoneColor._
import connectx.game.CellType
import connectx.game.VictoryChecker


class VictoryCheckerSpec extends AnyWordSpec  with Matchers{

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
      
      "find 4 left to right cross" in {
        val board = Board(7, 6)
        for (i <- 2.until(6)) 
          for (j <- i.until(7))
            if ((i+j) % 2 == 0)
              board.putStone(j, White)
            else
              board.putStone(j, Black)

        connectx.Console.printBoard(board)
        VictoryChecker.hasWon(board, White) shouldBe true
        VictoryChecker.hasWon(board, Black) shouldBe true
      }
    }
      
    //   "find 4 right to left cross" in {
    //     val board = Board(7, 6)
    //     for (i <- 2.until(6)) 
    //       for (j <- i.until(7))
    //         if ((i+j) % 2 == 0)
    //           board.putStone(j, White)
    //         else
    //           board.putStone(j, Black)

    //     connectx.Console.printBoard(board)
    //     VictoryChecker.hasWon(board, White) shouldBe true
    //     VictoryChecker.hasWon(board, Black) shouldBe true
    //   }
    // }
}