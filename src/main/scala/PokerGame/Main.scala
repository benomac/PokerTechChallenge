package PokerGame

import PokerGame.Deck.{dealHands, shuffleDeck}
import PokerGame.PlayAGame._
import PokerGame.Suit.{Clubs, Diamonds, Hearts, Spades}



object Main extends App {
  //player1 hand: Three of Hearts, Seven of Hearts, Eight of Hearts, Ten of Clubs, King of Diamonds
  //player2 hand: Ace of Hearts, Ace of Spades, Three of Spades, Nine of Hearts, King of Hearts
  val hand1 = Hand(Card(Three, Hearts), Card(Seven, Hearts), Card(Eight, Hearts), Card(Ten, Clubs), Card(King, Diamonds))
  val hand2 = Hand(Card(Ace, Hearts), Card(Ace, Spades), Card(Three, Spades), Card(Nine, Hearts), Card(King, Hearts))
  val deck = shuffleDeck
  val gameHands = dealHands(deck)
  val player1 = hand2
  val player2 = gameHands.p2

  println(s"player1 hand: ${player1.print}")
  println(s"player2 hand: ${player2.print}")
  println()

  println(s"player1 highest: ${getHighestScoreFromHand("player 1", player1).bestHand.print}")
  println(s"player2 highest: ${getHighestScoreFromHand("player 2", player2).bestHand.print}")
  println()

  println(determineWinner(player1Score, player2Score))
}
