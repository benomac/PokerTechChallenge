package PokerGame

import PokerGame.Deck.{dealHands, shuffleDeck}
import PokerGame.PlayAGame._
import PokerGame.Suit.Hearts



object Main extends App {
  val royalFlush = Hand(Card(King, Hearts), Card(Jack, Hearts), Card(Queen, Hearts), Card(Ten, Hearts), Card(Ace, Hearts))
  val straight = Hand(Card(Nine, Hearts), Card(Jack, Hearts), Card(Queen, Hearts), Card(Ten, Hearts), Card(Eight, Hearts))
  val deck = shuffleDeck
  val gameHands = dealHands(deck)
  println(deck)
  val player1 = gameHands.p1
  val player2 = gameHands.p2

  println(s"player1 hand: ${player1.print}")
  println(s"player2 hand: ${player2.print}")
  println()

  println(s"player1 highest: ${getHighestScoreFromHand("player 1", player1).bestHand.print}")
  println(s"player2 highest: ${getHighestScoreFromHand("player 2", player2).bestHand.print}")
  println()

  println(determineWinner(player1Score, player2Score))
}
