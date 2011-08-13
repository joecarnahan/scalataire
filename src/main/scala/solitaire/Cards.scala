package solitaire

import scala.util.Random

sealed case class Card (suit: Int, value: Int)

object Card {
  def suits = 1 to 4
  def values = 1 to 13

  def allCards = for (suit <- suits; value <- values) yield new Card(suit, value)

}

sealed case class Deck(cards: Seq[Card]) {
  def deal : (Card, Deck) = (cards.head, Deck(cards.tail))
}

object Deck {
  def shuffle(rng: Random) = Deck(rng.shuffle(Card.allCards))
}
