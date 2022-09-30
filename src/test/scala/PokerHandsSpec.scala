import PokerGame.Suit._
import PokerGame.{Card, Eight, Five, Hand, PokerHands, Two}
import PokerGame.PokerHands._
import org.scalatest.funsuite.AnyFunSuite

class PokerHandsSpec extends AnyFunSuite{
  test("fullHouseOrTwoPair returns 3 for a hand with 2 pairs") {
    val twoPairHand =
      Hand(Card(Eight, Clubs),
      Card(Eight, Spades),
      Card(Two, Hearts),
      Card(Two, Diamonds),
      Card(Two, Spades)).toSortedList.groupBy(c => c.cardValue)
    assert(fullHouseOrThreeOfAKind(twoPairHand) == 2)
  }
}
