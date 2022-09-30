package PokerGame
import scala.annotation.tailrec

object PokerHands {

  trait HandRankings {
      def handScore: Int
      def handRank: Int
      def print: String
      def toList: List[Card]
    }

  case class HighestCard(card: Card) extends HandRankings {
    override def handScore: Int = card.cardValue.rank

    override def handRank: Int = 1

    override def print: String = s"Highest card, ${card.cardValue} of ${card.suit.print}"

    override def toList: List[Card] = List(card)
  }

  case class Pair(c1: Card, c2: Card) extends HandRankings {

    override def handScore: Int = c1.cardValue.rank * 2

    override def handRank: Int = 2

    override def print: String = s"Pair, ${c1.cardValue}'s"

    override def toList: List[Card] = List(c1, c2)
  }

  case class TwoPairs(pair1: Pair, pair2: Pair) extends HandRankings {
    override def handScore: Int = pair1.handScore + pair2.handScore

    override def handRank: Int = 3

    override def print: String = s"Two pair ${pair1.c1.cardValue}'s and ${pair2.c1.cardValue}'s"

    override def toList: List[Card] = List(pair1.c1, pair1.c2, pair2.c1, pair2.c2)

  }

  case class ThreeOfAKind(c1: Card, c2: Card, c3: Card)  extends HandRankings {
    override def handScore: Int = c1.cardValue.rank * 3

    override def handRank: Int = 4

    override def print: String = s"Three of a kind, ${c1.cardValue}'s"

    override def toList: List[Card] = List(c1, c2, c3)
  }

  case class Straight(hand: Hand) extends HandRankings {
    val handList = hand.toSortedList
    override def handScore: Int = handList.flatMap(c => List(c.cardValue.rank)).sum

    override def handRank: Int = 5

    override def print: String = {
      val straight = handList match {
        case List(c1, c2, c3, c4, c5) => Straight(Hand(c1, c2, c3, c4, c5))
      }
      s"Straight, ${straight.hand.print}"
    }

    override def toList: List[Card] = handList
  }

  case class Flush(hand: Hand) extends HandRankings {
    val handList = hand.toSortedList

    override def handScore: Int = handList.flatMap(c => List(c.cardValue.rank)).sum

    override def handRank: Int = 6

    override def print: String = {
      val flush = handList match {
        case List(c1, c2, c3, c4, c5) => Flush(Hand(c1, c2, c3, c4, c5))
      }
      s"Flush, ${flush.hand.print}"
    }

    override def toList: List[Card] = handList
  }

  case class FullHouse(threeOfAKind: ThreeOfAKind, pair: Pair) extends HandRankings {
    override def handScore: Int = threeOfAKind.handScore + pair.handScore

    override def handRank: Int = 7

    override def print: String = s"Full house, ${threeOfAKind.print}, ${pair.print}"

    override def toList: List[Card] = List(threeOfAKind.toList, pair.toList).flatten
  }

  case class FourOfAKind(c1: Card, c2: Card, c3: Card, c4: Card) extends HandRankings {
    override def handScore: Int = c1.cardValue.rank * 4

    override def handRank: Int = 8

    override def print: String = s"Four of a kind, ${c1.cardValue.rank}'s"

    override def toList: List[Card] = List(c1, c2, c3, c4)
  }

  case class StraightFlush(hand: Hand) extends HandRankings {
    val handList = hand.toSortedList
    override def handScore: Int = handList.flatMap(c => List(c.cardValue.rank)).sum

    override def handRank: Int = 9

    override def print: String = {
      val straightFlush = handList match {
        case List(c1, c2, c3, c4, c5) => StraightFlush(Hand(c1, c2, c3, c4, c5))
      }
      s"Straight Flush, ${straightFlush.hand.c1.print}, " +
        s"${straightFlush.hand.c2.print}, " +
        s"${straightFlush.hand.c3.print}, " +
        s"${straightFlush.hand.c4.print}, " +
        s"${straightFlush.hand.c5.print}"
    }

    override def toList: List[Card] = handList
  }

  case class RoyalFlush(hand: Hand) extends HandRankings {
    val handList = hand.toSortedList

    override def handScore: Int = handList.flatMap(c => List(c.cardValue.rank)).sum

    override def handRank: Int = 10

    override def print: String = {
      val royalFlush = handList match {
        case List(c1, c2, c3, c4, c5) => StraightFlush(Hand(c5, c1, c2, c3, c4))
      }
      s"Royal Flush, Ace of ${royalFlush.hand.c1.suit.print}, " +
        s"${royalFlush.hand.c2.print}, " +
        s"${royalFlush.hand.c3.print}, " +
        s"${royalFlush.hand.c4.print}, " +
        s"${royalFlush.hand.c5.print}"
    }

    override def toList: List[Card] = handList
  }

  // TODO RoyalFlush


    @tailrec
    def fullHouseOrThreeOfAKind(m: Map[CardValue, List[Card]], count: Int = 0): Int = {
      if (m.isEmpty) count
      else if (m.head._2.length == 2 || m.head._2.length == 3) fullHouseOrThreeOfAKind(m.tail, count + 1)
      else fullHouseOrThreeOfAKind(m.tail, count)
    }

    @tailrec
    def getTwoPairs(m: Map[CardValue, List[Card]], acc: List[Pair] = Nil): HandRankings = {
      if (m.isEmpty) TwoPairs(acc.head, acc.last)
      else if (m.head._2.length == 2) {
        val c1 = m.head._2.head
        val c2 = m.head._2.last
        getTwoPairs(m.tail, acc ::: List(Pair(c1, c2)))
      }
      else getTwoPairs(m.tail, acc)
    }

    @tailrec
    def getTheFullHouse(m: Map[CardValue, List[Card]], pair: List[Pair] = Nil, three: List[ThreeOfAKind] = Nil): FullHouse = { // not safe
      if (m.isEmpty) {
        FullHouse(three.head, pair.head)
      }
      else {
        val c1 = Card(m.head._2.head.cardValue, m.head._2.head.suit)
        val c2 = Card(m.head._2(1).cardValue, m.head._2(1).suit)
        m.head._2.length match {
          case 2 =>
            getTheFullHouse(m.tail, pair ::: List(Pair(c1, c2)), three)
          case 3 =>
            val c3 = Card(m.head._2.last.cardValue, m.head._2.last.suit)
            getTheFullHouse(m.tail, pair, three ::: List(ThreeOfAKind(c1, c2, c3)))
          case _ => getTheFullHouse(m.tail, pair, three)

        }
      }
    }



    def checkForMultiples(hand: Hand): Option[HandRankings] = {
      val handMap: Map[CardValue, List[Card]] = hand.toSortedList.groupBy(c => c.cardValue)
      val theMultiple = handMap.valuesIterator.reduceLeft((x, y) => if (x.length > y.length) x else y).head.cardValue

      val m = handMap(theMultiple)
      println(10)
      handMap.size match {
        case 4 => Some(Pair(m.head, m.last))
        case 3 if fullHouseOrThreeOfAKind(handMap) != 2 => Some(ThreeOfAKind(m.head, m(1), m.last))
        case 3 => Some(getTwoPairs(handMap))
        case 2 if fullHouseOrThreeOfAKind(handMap) == 2 => Some(getTheFullHouse(handMap))
        case 2 => Some(FourOfAKind(m.head, m(1), m(2), m.last))
        case _ => Some(getHighestCard(hand.toSortedList))
      }
    }

    def getHighestCard(hand: List[Card]): HandRankings =
      HighestCard(hand.maxBy(_.cardValue.rank))


    def checkForFlush(hand: Hand): Option[Flush] = {
      val list = hand.toSortedList
      val suit = list.head.suit
      if (list.forall(c => c.suit == suit)) {
        list match {
          case List(c1, c2, c3, c4, c5) => Some(Flush(Hand(c1, c2, c3, c4, c5)))
          case _ => None
        }
      } else None
    }



    def checkForStraight(hand: Hand): Option[Straight] = {
      println(11)
      val list = hand.toSortedList.reverse
      @tailrec
      def check(l: List[Card]): Boolean = {
        println(12)
        l.length match {
          case 1 => true
          case _ => l match {
            case ::(head, tail) if head.cardValue.rank - tail.head.cardValue.rank == 1 =>
              println(s"$l 1st")
              check(tail)
            case ::(head, tail) if head.cardValue == King =>
              println(s"$l 2nd")
              list match {
                case List(c1, c2, c3, c4, c5) => check(List(Card(AceHigh, c5.suit), c1, c2, c3, c4))
              }
            case _ => false
          }
        }
      }
      if (check(list)) {
        list match {
          case List(c1, c2, c3, c4, c5) => Some(Straight(Hand(c1, c2, c3, c4, c5)))
        }
      } else None
    }

    def checkForStraightFlush(hand: Hand): Option[StraightFlush] = {
      if(checkForFlush(hand).isDefined && checkForStraight(hand).isDefined)
        hand.toSortedList.reverse match {
          case List(c1, c2, c3, c4, c5) => Some(StraightFlush(Hand(c1, c2, c3, c4, c5)))
        }
      else
        None
    }

    def checkForRoyalFlush(hand: Hand): Option[RoyalFlush] = {
      if (checkForStraightFlush(hand).isDefined) {
        hand.toSortedList.reverse match {
          case List(c1, c2, c3, c4, c5) if c1.cardValue == King =>
            Some(RoyalFlush(Hand(Card(AceHigh, c5.suit), c1, c2, c3, c4)))
          case _ => None
        }
      } else
          None
    }
}
