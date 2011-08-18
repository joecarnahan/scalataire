package solitaire

sealed case class Suit(cards: List[Card])

sealed case class GameState(deck: Deck, stack: Stack, suits: List[Suit], piles: List[List[Card]]) {

  def nl = System.getProperty("line.separator")

  override def toString = 
    deck + nl + stack + nl + suits.mkString(nl) + nl + piles.mkString(nl)
    
  def nextMoves: Iterable[GameState] = deal :: putUpCards ++ moveCards

  def deal: GameState = deal(3)
    
  def deal(n: Int): GameState = 
    if (n == 0)
      this
    else if (deck.cards.isEmpty) {
      GameState(Deck(stack.flip), Stack(), suits, piles)
    }
    else {
      val (topCard, remainingCards) = deck.deal
      GameState(remainingCards, stack.put(topCard), suits, piles).deal(n - 1)
    }

  def putUpCards = List[GameState]() // TODO

  def moveCards = List[GameState]() // TODO

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
                   List.fill(4)(Suit(List[Card]())),
                   List.fill(7)(List[Card]())))
  }

}
