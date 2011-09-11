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
 * A stack is a list of cards onto which cards can be placed and that can
 * be flipped over.  A common use for a card stack would be a discard pile.
 */
sealed case class Stack(cards: List[Card]) {
  def top = if (cards.isEmpty) None else Some(cards.head)
  def put(card: Card) = Stack(card :: cards)
  def flip = cards.reverse
}

object Stack {
  def apply(): Stack = Stack(List[Card]())
}

// RESUME HERE
sealed case class Pile(faceup: List[Card], facedown: List[Card]) {

  override def toString = faceup.toString + " on top of " + facedown.toString

  def getTopCard: Option[Card] = faceup.headOption

  def remove(cards: List[Card]): Pile = {
    def removeAllOrNothing(cardsToRemove: List[Card], currentPile: Pile, originalPile: Pile): Pile =
      if (cardsToRemove.isEmpty)
        if (currentPile.faceup.isEmpty && !currentPile.facedown.isEmpty)
          Pile(List(currentPile.facedown.head), currentPile.facedown.tail)
        else
          currentPile
      else if (currentPile.faceup.isEmpty)
        originalPile
      else if (cardsToRemove.head == currentPile.faceup.head)
        removeAllOrNothing(cardsToRemove.tail, Pile(currentPile.faceup.tail, currentPile.facedown), originalPile)
      else
        originalPile
    removeAllOrNothing(cards, this, this)
  }

  def deal(card: Card): Pile = Pile(List(card), faceup ++ facedown)

  def put(cards: List[Card]): Pile = 
    if (faceup.isEmpty)
      Pile(cards, List[Card]())
    else
      Pile(cards ++ faceup, facedown)

  def substacks: List[List[Card]] = {
    def substacks(cards: List[Card], stacks: List[List[Card]]): List[List[Card]] =
      cards match {
        case (_ :: rest) => substacks(rest, cards.reverse :: stacks)
        case _ => stacks
      }
    substacks(faceup.reverse, List[List[Card]]())
  }

}

