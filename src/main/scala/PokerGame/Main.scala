package PokerGame

import PokerGame.Deck.{dealHands, shuffleDeck}
import PokerGame.PlayAGame._


object Main extends App {
  val deck = shuffleDeck

  val gameHands = dealHands(deck)
  println(deck)
  val player1 = gameHands.p1
  val player2 = gameHands.p2

  println(s"player1 hand: ${player1.print}")
  println(s"player2 hand: ${player2.print}")


  println(s"player1 highest: ${getHighestScoreFromHand("player 1", player1).bestHand.print}")
  println(s"player2 highest: ${getHighestScoreFromHand("player 2", player2).bestHand.print}")


  println(determineWinner(player1Score, player2Score))
}
