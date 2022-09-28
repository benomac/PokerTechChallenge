package PokerGame

import scala.util.Random
import PokerGame.Suit._

import scala.annotation.unused

object Deck {
  val suits: List[Suit] = List(Spades, Clubs, Hearts, Diamonds)
  val cardValues: List[CardValue] = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

  val deck: List[Card] = for {
    card <- cardValues
    suit <- suits
  } yield Card(card, suit)

  def shuffleDeck: List[Card] = {
    Random.shuffle(deck)
  }

  case class GameHands(p1: Hand, p2: Hand)

  def dealHands(shuffledDeck: List[Card]): GameHands = {
    val p1 = shuffledDeck.take(10).filter(x => shuffledDeck.indexOf(x) % 2 == 0) match {
      case List(c1, c2, c3, c4, c5) => Hand(c1, c2, c3, c4, c5)
    }
    val p2 = shuffledDeck.take(10).filter(x => shuffledDeck.indexOf(x) % 2 != 0) match {
      case List(c1, c2, c3, c4, c5) => Hand(c1, c2, c3, c4, c5)
    }
    GameHands(p1, p2)

  }


}
