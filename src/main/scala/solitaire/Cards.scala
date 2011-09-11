/**
 * Code for representing playing cards, which could be used for any type of
 * card game.
 *
 * @author Joe Carnahan
 */

package solitaire

/**
 * Playing cards are represented using integers to indicate the suit and the
 * value.
 *
 * @todo Need to determine if using Ints is really faster than using enums would be
 */
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

/**
 * A deck is simply a list of cards that can be shuffled and from which the top
 * card can be dealt.
 */
sealed case class Deck(cards: Seq[Card]) {
  def deal : (Card, Deck) = (cards.head, Deck(cards.tail))
}

object Deck {
  val rng = new scala.util.Random
  def shuffle = Deck(rng.shuffle(Card.allCards))
}

/**
 * A pile is a list of cards onto which cards can be placed and that can
 * be flipped over.  A common use for a card pile would be a discard pile.
 */
sealed case class Pile(cards: List[Card]) {
  def top = if (cards.isEmpty) None else Some(cards.head)
  def put(card: Card) = Pile(card :: cards)
  def flip = cards.reverse
}

object Pile {
  def apply(): Pile = Pile(List[Card]())
}

