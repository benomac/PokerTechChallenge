package PokerGame

case class Hand (c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) {
  def print: String = {
    this.toSortedList match {
      case List(c1, c2, c3, c4, c5) => s"${c1.print}, ${c2.print}, ${c3.print}, ${c4.print}, ${c5.print}"
    }

  }
  def toSortedList: List[Card] =
      List(c1, c2, c3, c4, c5).sortBy(_.cardValue.rank)

}
