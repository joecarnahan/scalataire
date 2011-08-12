package solitaire

sealed case class Card (suit: Int, value: Int)

object Card {

  def suits = 1 to 4

  def values = 1 to 13

  def allCards = for (suit <- suits; value <- values) yield new Card(suit, value)

}

