package solitaire

sealed case class GameState(deck: Deck, stack: Stack, suits: List[List[Card]], piles: List[List[Card]]) {

  def nl = System.getProperty("line.separator")

  override def toString = 
    deck + nl + stack + nl + suits.mkString(nl) + nl + piles.mkString(nl)
    
  def nextMoves: Iterable[GameState] = List(draw) ++ putUpCards ++ moveCards

  def draw: GameState = draw(3)
    
  def draw(n: Int): GameState = 
    if (n == 0)
      this
    else if (deck.cards.isEmpty) {
      GameState(Deck(stack.flip), Stack(), suits, piles)
    }
    else {
      val (topCard, remainingCards) = deck.deal
      GameState(remainingCards, stack.put(topCard), suits, piles).draw(n - 1)
    }

  def putUpCards: Iterable[GameState] =
    (stack.top ++ piles.flatMap(_.headOption)).flatMap(putUpCard(_))

  def putUpCard(card: Card): Option[GameState] =
    if (topOfSuit(card.suit) == (card.value - 1))
      Some(GameState(deck,
                     Stack(removeIfTopCard(stack.cards, card)),
                     suits.patch(card.suit, List(card :: suits(card.suit)), 1),
                     piles.map(removeIfTopCard(_, card))))
    else
      None

  def topOfSuit(suit: Int): Int = suits(suit).headOption.map(_.value).getOrElse(-1)

  def removeIfTopCard(cards: List[Card], card: Card): List[Card] =
    if (cards.isEmpty || (cards.head != card))
      cards
    else
      cards.tail

  def moveCards: Iterable[GameState] = List[GameState]() // TODO

}


object Game {

  def deal(initial: GameState) = {
    def dealCount(givenState: GameState, i: Int, j: Int): GameState = {
      if (i >= 7)
        givenState
      else if (j > i)
        dealCount(givenState, i+1, 0)
      else {
        val (topCard, remainingCards) = givenState.deck.deal
        dealCount(GameState(remainingCards,
                            givenState.stack,
                            givenState.suits,
                            givenState.piles.patch(i, List(topCard :: givenState.piles(i)), 1)),
                  i,
                  j+1)
      }
    }
    dealCount(initial, 0, 0)
  }
        

  def apply() = {
    deal(GameState(Deck.shuffle, 
                   Stack(),
                   List.fill(4)(List[Card]()),
                   List.fill(7)(List[Card]())))
  }

}
