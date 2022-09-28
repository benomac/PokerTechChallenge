package PokerGame

trait Suit {
  def print: String
}

object Suit {
  object Spades extends Suit {
    override def print: String = "Spades"
  }

  object Clubs extends Suit {
    override def print: String = "Clubs"
  }

  object Hearts extends Suit {
    override def print: String = "Hearts"
  }

  object Diamonds extends Suit {
    override def print: String = "Diamonds"
  }
}

