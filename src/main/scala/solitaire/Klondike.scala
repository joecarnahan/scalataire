package solitaire

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

  def moveableStacks: List[List[Card]] = {
    def moveableStacks(cards: List[Card], stacks: List[List[Card]]): List[List[Card]] =
      cards match {
        case (_ :: rest) => moveableStacks(rest, cards.reverse :: stacks)
        case _ => stacks
      }
    moveableStacks(faceup.reverse, List[List[Card]]())
  }

}

sealed case class GameState(deck: Deck, stack: Stack, suits: List[List[Card]], piles: List[Pile]) {

  def nl = System.getProperty("line.separator")

  override def toString = 
    deck + nl + stack + nl + suits.mkString(nl) + nl + piles.mkString(nl)
    
  def nextStates(movesToSkip: GameState => Boolean): Iterable[GameState] = 
    (List(draw) ++ putUpCards ++ moveCards).filterNot(movesToSkip)

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
    (stack.top ++ piles.flatMap(_.getTopCard)).flatMap(putUpCard(_))

  def putUpCard(card: Card): Option[GameState] =
    if (topOfSuit(card.suit) == (card.value - 1))
      Some(GameState(deck,
                     Stack(removeIfTopCard(stack.cards, card)),
                     suits.patch(card.suit, List(card :: suits(card.suit)), 1),
                     piles.map(_.remove(List(card)))))
    else
      None

  def topOfSuit(suit: Int): Int = suits(suit).headOption.map(_.value).getOrElse(-1)

  def removeIfTopCard(cards: List[Card], card: Card): List[Card] =
    if (cards.isEmpty || (cards.head != card))
      cards
    else
      cards.tail

  def moveCards: Iterable[GameState] = allMoveableStacks.flatMap(buildPossibleMoves(_))

  def allMoveableStacks: Iterable[List[Card]] = 
    stack.top.map(List(_)) ++ piles.flatMap(_.moveableStacks)

  def buildPossibleMoves(cardsToMove: List[Card]): Iterable[GameState] = 
    (0 until Game.numberOfPiles).flatMap(moveCardsToPile(_, cardsToMove))

  def moveCardsToPile(index: Int, cardsToMove: List[Card]): Option[GameState] = 
    if (goesOn(cardsToMove.lastOption, piles(index).faceup.headOption))
      Some(GameState(deck,
                     if (cardsToMove.length == 1)
                       Stack(removeIfTopCard(stack.cards, cardsToMove.head))
                     else 
                       stack,
                     suits,
                     piles.map(_.remove(cardsToMove)).
                       patch(index, List(piles(index).put(cardsToMove)), 1)))
    else
      None

  def goesOn(a: Option[Card], b: Option[Card]): Boolean =
    a match {
      case Some(aCard) => b match {
        case Some(bCard) => 
          ((aCard.value + 1) == bCard.value) &&
          ((aCard.isBlack && bCard.isRed) || (aCard.isRed && bCard.isBlack))
        case None =>
          aCard.value == Card.king
      }
      case None => false
    }

  def moveSequence(moves: Int): List[GameState] = {
    val previousMoves = new scala.collection.mutable.HashSet[GameState]
    def moveSequence(current: GameState, movesLeft: Int, pastSequence: List[GameState]): List[GameState] =
      if (movesLeft <= 0)
        pastSequence.reverse
      else {
        val nextStatesResult = current.nextStates(previousMoves.contains(_))
        if (nextStatesResult.isEmpty)
          pastSequence.reverse
        else {
          val nextMove = nextStatesResult.last
          previousMoves += nextMove
          moveSequence(nextMove, movesLeft - 1, nextMove :: pastSequence)
        }
      }
    previousMoves += this
    moveSequence(this, moves, List(this))
  }

  def isWin = suits.filter(_.size != Card.king).isEmpty

}

sealed case class GameHistory(state: GameState, pastStates: List[GameState]) {

  def allStates: List[GameState] = state :: pastStates

  def addAllNextMoves(startingList: List[GameHistory], movesToSkip: GameState => Boolean): List[GameHistory] =
    state.nextStates(movesToSkip).map(GameHistory(_, allStates)).
      foldLeft(startingList)((histories: List[GameHistory], newHistory: GameHistory) => newHistory :: histories)

}

object Game {

  val numberOfPiles = 7

  def deal(initial: GameState) = {
    def dealCount(givenState: GameState, i: Int, j: Int): GameState = {
      if (i >= numberOfPiles)
        givenState
      else if (j > i)
        dealCount(givenState, i+1, 0)
      else {
        val (topCard, remainingCards) = givenState.deck.deal
        dealCount(GameState(remainingCards,
                            givenState.stack,
                            givenState.suits,
                            givenState.piles.patch(i, List(givenState.piles(i).deal(topCard)), 1)),
                  i,
                  j+1)
      }
    }
    dealCount(initial, 0, 0)
  }

  def newGame: GameState =
    deal(GameState(Deck.shuffle, 
                   Stack(),
                   List.fill(Card.suits.length)(List[Card]()),
                   List.fill(numberOfPiles)(Pile(List[Card](),List[Card]()))))

  def playGame: List[List[GameState]] = {
    val previousStates = new scala.collection.mutable.HashSet[GameState]
    def playGameRec(statesToTry: List[GameHistory], pastWins: List[List[GameState]]): List[List[GameState]] =
      statesToTry match {
        case (nextToTry :: restToTry) => {
          previousStates += nextToTry.state
          if (nextToTry.state.isWin)
/* debug */ { println("Tried " + previousStates.size.toString + " states: Got a win, backtracking to explore other possibilities")
            playGameRec(restToTry, nextToTry.allStates.reverse :: pastWins)
/* debug */ }
          else
/* debug */ {
/* debug */   val newListToTry = nextToTry.addAllNextMoves(restToTry, previousStates.contains(_))
/* debug */   if ((previousStates.size % 100) == 0) println("Tried " + previousStates.size.toString + " states: Adding " + (newListToTry.size - restToTry.size).toString + " states to get a total of " + newListToTry.size.toString + " states to try")
            playGameRec(nextToTry.addAllNextMoves(restToTry, previousStates.contains(_)), pastWins)
            //playGameRec(nextToTry.addAllNextMoves(restToTry, previousStates.contains(_)), pastWins)
/* debug */ }
        }
/* debug */ case _ => {
/* debug */   println("Tried " + previousStates.size.toString + " states: No states left to try\n")
/* debug */   pastWins
        // case _ => pastWins
/* debug */ }
      }
    playGameRec(List(GameHistory(newGame, List[GameState]())), List[List[GameState]]())
  }

}
