package PokerGame

import PokerGame.Main.{player1, player2}
import PokerGame.PokerHands._

object PlayAGame {

  case class PlayerScores(player: String, hand: Hand, handRank: Int, handScore: Int, bestHand: HandRankings)

  def getHighestScoreFromHand(player: String, hand: Hand): PlayerScores = {
    val MaxFromList: HandRankings = List(
      checkForMultiples(hand),
      checkForStraight(hand),
      checkForFlush(hand),
      checkForStraightFlush(hand),
      Some(HighestCard(hand.toSortedList.maxBy(_.cardValue.rank)))
    ).flatten.maxBy(_.handRank)
    val handRank = MaxFromList.handRank
    val handScore = MaxFromList.handScore
    PlayerScores(player, hand, handRank, handScore, MaxFromList)
  }

  val player1Score: PlayerScores = getHighestScoreFromHand("Player 1", player1)
  val player2Score: PlayerScores = getHighestScoreFromHand("Player 2", player2)

  def removeBestHandFromHand(hand: PlayerScores): List[Card] = {
    for {
      i <- hand.bestHand.toList
    } yield hand.hand.toSortedList.filter(c => c.cardValue != i.cardValue)
  }.flatten

  def checkForHighestCard(p1Hand: PlayerScores, p2Hand: PlayerScores): String = {
    val p1RestOfHand = removeBestHandFromHand(p1Hand)
    val p2RestOfHand = removeBestHandFromHand(p2Hand)
    if ( {
      for {
        i <- p1RestOfHand
      } yield p2RestOfHand.filter(c => c.cardValue != i.cardValue)
    }.flatten == Nil
    ) "draw!"
    else if (getHighestCard(p1RestOfHand).handScore > getHighestCard(p2RestOfHand).handScore)
      s"Player 1 wins with the ${getHighestCard(p1RestOfHand).print}"
    else
      s"Player 2 wins with the ${getHighestCard(p2RestOfHand).print}"

  }

  def determineWinner(p1: PlayerScores, p2: PlayerScores): String = {
    (p1, p2) match {
      case (p1, p2) if p1.handRank > p2.handRank => s"Player 1 wins with ${p1.bestHand.print}"
      case (p1, p2) if p1.handRank < p2.handRank => s"Player 2 wins with ${p2.bestHand.print}"
      case (p1, p2) if p1.handRank == p2.handRank && p1.handScore != p2.handScore =>
        val maxP: PlayerScores = List(p1, p2).maxBy(_.handScore)
        s"${maxP.player} wins with ${maxP.bestHand.print}"
      case (p1, p2) if p1.handRank == p2.handRank && p1.handScore == p2.handScore =>
        checkForHighestCard(p1, p2)
    }
  }
}
