package solitaire

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
    stack.top.map(List(_)) ++ piles.flatMap(_.substacks)

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

  def isWin = suits.filter(_.size != (Card.king + 1)).isEmpty

}

sealed case class GameHistory(state: GameState, pastStates: List[GameState]) {

  def allStates: List[GameState] = state :: pastStates

  def getAllNextStates(movesToSkip: GameState => Boolean): Iterable[GameHistory] =
    state.nextStates(movesToSkip).map(GameHistory(_, allStates))

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
    val pendingStates = new scala.collection.mutable.HashSet[GameState]

    /* debug */
    val stepSize = 50000
    var timeStamp = System.currentTimeMillis
    def average(statesToTry: Iterable[GameHistory]): Int = 
      if (statesToTry.isEmpty)
        0
      else
        statesToTry.foldLeft(0)(_ + _.pastStates.size) / statesToTry.size
    def max(statesToTry: Iterable[GameHistory]): Int =
      if (statesToTry.isEmpty)
        0
      else
        statesToTry.foldLeft(0)((a: Int, h: GameHistory) => a.max(h.pastStates.size))
    def printState(statesToTry: Iterable[GameHistory], message: String) = {
      val currentTime = System.currentTimeMillis
      val rate = (stepSize.asInstanceOf[Double] / ((currentTime - timeStamp) / 1000.0)).round
      timeStamp = currentTime
      println("Tried " + previousStates.size.toString + " states at " + rate + " states/s, with " +
              statesToTry.size.toString + " states left to try with these sizes: avg = " + 
              average(statesToTry).toString + ", max = " + max(statesToTry).toString + ": " + message)
    }
    /* end debug */

    def playGameRec(statesToTry: Iterable[GameHistory], pastWins: List[List[GameState]]): List[List[GameState]] =
      statesToTry match {
        case (nextToTry :: restToTry) => 
          if (nextToTry.state.isWin)
/* debug */ { printState(statesToTry, ("***** Got a win! *****"))
              playGameRec(restToTry, nextToTry.allStates.reverse :: pastWins)
/* debug */ }
          else {
            pendingStates -= nextToTry.state
            previousStates += nextToTry.state
            val newListToTry: Iterable[GameHistory] = 
              nextToTry.getAllNextStates(x => previousStates.contains(x) || pendingStates.contains(x))
/* debug */ if ((previousStates.size % stepSize) == 0) printState(statesToTry, "Adding " + newListToTry.size.toString + " states")
/* debug */ if (((previousStates.size % (stepSize / 100)) == 0) && (System.in.available() > 0)) {
/* debug */   val nl = System.getProperty("line.separator")
/* debug */   println(nl + "After " + previousStates.size.toString + " states, current state:" + nl + nextToTry.state + nl)
/* debug */   val nextMoves = nextToTry.state.nextStates(_ => false)
/* debug */   val nextNewMoves = nextToTry.state.nextStates(x => previousStates.contains(x) || pendingStates.contains(x))
/* debug */   if (nextNewMoves.size != newListToTry.size) sys.error("Inconsistent number of new states to try")
/* debug */   println("Skipping " + (nextMoves.size - nextNewMoves.size) + " states that were already tried")
/* debug */   println("Remaining possible moves:" + nl)
/* debug */   nextNewMoves.map((g: GameState) => {
/* debug */     if (g.isWin) println("This is a win!!!!!")
/* debug */     println(g.toString + nl)
/* debug */   })
/* debug */   println("Exiting")
/* debug */   sys.exit(0)
/* debug */ }
            newListToTry.map((g: GameHistory) => pendingStates.add(g.state))
            playGameRec(newListToTry ++ restToTry, pastWins)
          }
/* debug */ case _ => {
/* debug */   printState(statesToTry, "No moves left to try"); println()
/* debug */   pastWins
        // case _ => pastWins
/* debug */ }
      }
    playGameRec(List(GameHistory(newGame, List[GameState]())), List[List[GameState]]())
  }

}
