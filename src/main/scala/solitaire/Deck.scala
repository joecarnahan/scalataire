package solitaire

import scala.util.Random

sealed case class Deck(cards: Seq[Card]) {
  def deal : (Card, Deck) = (cards.head, Deck(cards.tail))
}

object Deck {
  def shuffle(rng: Random) = Deck(rng.shuffle(Card.allCards))
}
