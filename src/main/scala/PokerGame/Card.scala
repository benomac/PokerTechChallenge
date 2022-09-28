package PokerGame

case class Card(cardValue: CardValue, suit: Suit) {
  def print: String = s"$cardValue of ${suit.print}"
}

