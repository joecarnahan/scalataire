package solitaire

import scala.util.Random

sealed case class Card (suit: Int, value: Int) {
  def isBlack = suit % 2 == 0
  def isRed = !isBlack
}

object Card {
  val ace = 0
  val king = 12
  def suits = 0 to 3
  def values = ace to king
  def allCards = for (suit <- suits; value <- values) yield new Card(suit, value)
}

sealed case class Deck(cards: Seq[Card]) {
  def deal : (Card, Deck) = (cards.head, Deck(cards.tail))
}

object Deck {
  val rng = new Random
  def shuffle = Deck(rng.shuffle(Card.allCards))
}

sealed case class Stack(cards: List[Card]) {
  def top = if (cards.isEmpty) None else Some(cards.head)
  def put(card: Card) = Stack(card :: cards)
  def flip = cards.reverse
}

object Stack {
  def apply(): Stack = Stack(List[Card]())
}

