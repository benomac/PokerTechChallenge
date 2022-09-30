import PokerGame.Deck.{dealHands, shuffleDeck}
import org.scalatest.funsuite.AnyFunSuite

class DeckSpec extends AnyFunSuite {


  test("shuffleDeck produces a List of unique Cards of length 52") {
    assert(shuffleDeck.toSet.size == 52)
  }

  test("dealHands produces a GameHands of 2 unique Hands") {
    val hands = dealHands(shuffleDeck)
    for {
      i <- hands.p1.toSortedList
    } yield assert(hands.p2.toSortedList.filter(c => c == i) == Nil)
  }

}
