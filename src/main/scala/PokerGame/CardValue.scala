package PokerGame

trait CardValue {
  def rank: Int
}

case object AceHigh extends CardValue {
  override def rank = 14
}

case object AceLow extends CardValue {
  override def rank = 1
}
case object Two extends CardValue {
  override def rank = 2
}
case object Three extends CardValue {
  override def rank = 3
}
case object Four extends CardValue {
  override def rank = 4
}
case object Five extends CardValue {
  override def rank = 5
}
case object Six extends CardValue {
  override def rank = 6
}
case object Seven extends CardValue {
  override def rank = 7
}
case object Eight extends CardValue {
  override def rank = 8
}
case object Nine extends CardValue {
  override def rank = 9
}
case object Ten extends CardValue {
  override def rank = 10
}
case object Jack extends CardValue {
  override def rank = 11
}
case object Queen extends CardValue {
  override def rank = 12
}
case object King extends CardValue {
  override def rank = 13
}
