/**
 * Code for playing the game of Klondike Solitaire.
 *
 * @author Joe Carnahan
 */

package solitaire

/**
 * Represents one of the seven stacks of cards on the table.  Each stack is
 * always either empty or otherwise has one or more faceup cards on top of
 * zero or more facedown cards.
 */
sealed case class Stack(faceup: List[Card], facedown: List[Card]) {

  override def toString = faceup.toString + " on top of " + facedown.toString

  /**
   * Gets the top faceup card, if any.
   */
  def getTopCard: Option[Card] = faceup.headOption

  /**
   * Returns a new version of this stack without the given cards if the given 
   * cards are on top of the faceup pile, otherwise returns this stack 
   * unchanged.  If the given cards make up the entire faceup pile for this
   * stack, then the top facedown card (if any) is flipped faceup.
   */
  def remove(cards: List[Card]): Stack = {
    
    def removeAllOrNothing(cardsToRemove: List[Card], currentStack: Stack, originalStack: Stack): Stack =
      if (cardsToRemove.isEmpty)
        if (currentStack.faceup.isEmpty && !currentStack.facedown.isEmpty)
          Stack(List(currentStack.facedown.head), currentStack.facedown.tail)
        else
          currentStack
      else if (currentStack.faceup.isEmpty)
        originalStack
      else if (cardsToRemove.head == currentStack.faceup.head)
        removeAllOrNothing(cardsToRemove.tail, Stack(currentStack.faceup.tail, currentStack.facedown), originalStack)
      else
        originalStack

    removeAllOrNothing(cards, this, this)
  }

  /**
   * Adds the given card to this stack to create a new stack.  Note that when 
   * the cards are dealt at the start of the game, each stack should only have
   * one faceup card.  So, this method takes any faceup cards and turns them
   * facedown while also accepting the given card faceup.
   */
  def deal(card: Card): Stack = Stack(List(card), faceup ++ facedown)

  /**
   * Puts the given cards onto this stack to create a new stack.
   */
  def put(cards: List[Card]): Stack = 
    if (faceup.isEmpty)
      Stack(cards, List[Card]())
    else
      Stack(cards ++ faceup, facedown)

  /**
   * Gets a list containing all of the lists of cards that could be taken off of
   * the top of the faceup pile.
   */
  def substacks: List[List[Card]] = {

    def substacks(cards: List[Card], stacks: List[List[Card]]): List[List[Card]] =
      cards match {
        case (_ :: rest) => substacks(rest, cards.reverse :: stacks)
        case _ => stacks
      }

    substacks(faceup.reverse, List[List[Card]]())
  }

}

/**
 * A game of Klondike Solitaire is represented by a deck, a pile of cards that
 * are dealt from the deck, the seven stacks of cards on the table, and the four
 * piles where cards of each suit must be placed in order to win the game.
 */
sealed case class GameState(deck: Deck, pile: Pile, suits: List[List[Card]], stacks: List[Stack]) {

  def nl = System.getProperty("line.separator")

  override def toString = 
    deck + nl + pile + nl + suits.mkString(nl) + nl + stacks.mkString(nl)
    
  /**
   * Gets all of the legal game states that could be reached from this state in
   * a single move, omitting any states for which the given predicate is true.  
   * The possible moves include dealing more cards from the deck, putting cards
   * up onto their suit piles, or moving cards onto a stack from either the pile
   * of dealt cards or from one of the others stacks.
   * 
   * @param movesToSkip
   *          A function to indicate which moves should be skipped.  One obvious
   *          choice for this function would be a test to determine if a state
   *          has already been visited, but other filters could be used.
   */
  def nextStates(movesToSkip: GameState => Boolean): Iterable[GameState] = 
    (putUpCards ++ moveCards ++ List(draw)).filterNot(movesToSkip)

  /**
   * Deals the top three cards off of the deck or flips the pile over to make a
   * new deck if the deck is empty.
   */
  def draw: GameState = {

    def draw(current: GameState, n: Int): GameState = 
      if ((n == 0) || current.deck.cards.isEmpty)
        current
      else {
        val (topCard, remainingCards) = current.deck.deal
        draw(GameState(remainingCards, current.pile.put(topCard), current.suits, current.stacks), n - 1)
      }

    if (deck.cards.isEmpty)
      GameState(Deck(pile.flip), Pile(), suits, stacks)
    else
      draw(this, 3)
  }

  /**
   * Tries putting up each of the topmost faceup cards onto the suit piles.
   */
  def putUpCards: Iterable[GameState] =
    (pile.top ++ stacks.flatMap(_.getTopCard)).flatMap(putUpCard(_))

  /**
   * Removes the given card from the given list if the given card is at the
   * head of the given list.
   */
  def removeIfTopCard(cards: List[Card], card: Card): List[Card] =
    if (cards.isEmpty || (cards.head != card))
      cards
    else
      cards.tail

  /**
   * Tries to put the given card onto its respective suit pile.
   */
  def putUpCard(card: Card): Option[GameState] = {

    def topOfSuit(suit: Int): Int = suits(suit).headOption.map(_.value).getOrElse(-1)

    if (topOfSuit(card.suit) == (card.value - 1))
      Some(GameState(deck,
                     Pile(removeIfTopCard(pile.cards, card)),
                     suits.patch(card.suit, List(card :: suits(card.suit)), 1),
                     stacks.map(_.remove(List(card)))))
    else
      None
  }

  /**
   * Tries to move cards onto a stack from either the pile or from another stack.
   */
  def moveCards: Iterable[GameState] = {
    
    /**
     * Builds a list of all possible piles of cards that could be moved around.
     */
    def allMoveableStacks: Iterable[List[Card]] = 
      pile.top.map(List(_)) ++ stacks.flatMap(_.substacks)
  
    /**
     * Returns a sequence of game states that could legally result from moving
     * the given pile of cards by trying to put the given pile on each of the
     * stacks on the table.
     */
    def buildPossibleMoves(cardsToMove: List[Card]): Iterable[GameState] = 
      (0 until Game.numberOfStacks).flatMap(moveCardsToStack(_, cardsToMove))
  
    /**
     * Tries to put the given cards onto the stack with the given index.
     * 
     * <em>Note:</em> The use of an index here is deliberate, even though it's
     * admittedly not good functional programming style.  The problem with not 
     * using an index and just trying to map over the list of stacks is that
     * things get tricky when there are two or more legal moves that could be
     * made by moving the same pile of cards.
     */
    def moveCardsToStack(index: Int, cardsToMove: List[Card]): Option[GameState] = 
      if (goesOn(cardsToMove.lastOption, stacks(index).faceup.headOption))
        Some(GameState(deck,
                       if (cardsToMove.length == 1)
                         Pile(removeIfTopCard(pile.cards, cardsToMove.head))
                       else 
                         pile,
                       suits,
                       stacks.map(_.remove(cardsToMove)).
                         patch(index, List(stacks(index).put(cardsToMove)), 1)))
      else
        None
  
    /**
     * Function to test if one card can go on top of another, which is true if
     * the top card is a different color than the bottom card and if the top
     * card's value is exactly one less than the bottom card's value.
     */
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

    allMoveableStacks.flatMap(buildPossibleMoves(_))
  }

  /**
   * Checks if this game state is the winning state by looking at the suit piles
   * and seeing if all of them have 13 cards in them.
   */
  def isWin = suits.filter(_.size != (Card.king + 1)).isEmpty

}

/**
 * Represents a game state and the sequence of moves taken to reach that state.
 */
sealed case class GameHistory(state: GameState, pastStates: List[GameState]) {

  def allStates: List[GameState] = state :: pastStates

  def getAllNextStates(movesToSkip: GameState => Boolean): Iterable[GameHistory] =
    state.nextStates(movesToSkip).map(GameHistory(_, allStates))

}

/**
 * Klondike Solitaire code that does not belong to any other class.
 */
object Game {

  /**
   * The number of stacks on the table.
   */
  val numberOfStacks = 7

  /**
   * Given a game state with a deck, deals the stacks onto the table.
   * The first stack has one card, the second stack has two, and so on up
   * through the seventh stack that has seven cards.
   */
  def dealStacks(initial: GameState) = {
    def dealStacksCount(givenState: GameState, i: Int, j: Int): GameState = {
      if (i >= numberOfStacks)
        givenState
      else if (j > i)
        dealStacksCount(givenState, i+1, 0)
      else {
        val (topCard, remainingCards) = givenState.deck.deal
        dealStacksCount(GameState(remainingCards,
                                  givenState.pile,
                                  givenState.suits,
                                  givenState.stacks.patch(i, List(givenState.stacks(i).deal(topCard)), 1)),
                  i,
                  j+1)
      }
    }
    dealStacksCount(initial, 0, 0)
  }

  /**
   * Creates a new game by shuffling a full deck of cards and dealing out the
   * stacks.
   */
  def newGame: GameState =
    dealStacks(GameState(Deck.shuffle, 
                         Pile(),
                         List.fill(Card.suits.length)(List[Card]()),
                         List.fill(numberOfStacks)(Stack(List[Card](),List[Card]()))))

  /**
   * Explores all of the possible moves for the given game, ignoring chains of
   * moves that are larger than the given limit.
   *
   * @param sizeLimit
   *          the longest chain of moves to consider
   */
  def playGame(sizeLimit: Int): List[List[GameState]] = {

    import scala.collection.mutable.Set

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
    def printState(size: Int, statesToTry: Iterable[GameHistory], message: String) = {
      val currentTime = System.currentTimeMillis
      val rate = (stepSize.asInstanceOf[Double] / ((currentTime - timeStamp) / 1000.0)).round
      timeStamp = currentTime
      println("After " + size.toString + " states at " + rate + " states/s, " +
              statesToTry.size.toString + " states are queued up with these sizes: avg = " + 
              average(statesToTry).toString + ", max = " + max(statesToTry).toString + ": " + message)
    }

    def playGameRec(statesToTry: Iterable[GameHistory], pastWins: List[List[GameState]], 
                    knownStates: Set[GameState], counter: Int): List[List[GameState]] =
      statesToTry match {
        case (nextToTry :: restToTry) => 
          if (nextToTry.state.isWin) {
            printState(counter, statesToTry, ("***** Got a win! *****"))
            playGameRec(restToTry, nextToTry.allStates.reverse :: pastWins, knownStates, counter + 1)
          }
          else {
            if ((counter % stepSize) == 0) printState(counter, statesToTry, "")
            if (nextToTry.pastStates.size >= sizeLimit) {
              playGameRec(restToTry, pastWins, knownStates, counter + 1)
            }
            else {
              val newListToTry: Iterable[GameHistory] = nextToTry.getAllNextStates(knownStates.contains(_))
              if (((counter % (stepSize / 100)) == 0) && (System.in.available() > 0)) {
                val nl = System.getProperty("line.separator")
                println(nl + "After considering " + counter + " states, current state:" + nl + nextToTry.state + nl)
                val nextMoves = nextToTry.state.nextStates(_ => false)
                val nextNewMoves = nextToTry.state.nextStates(knownStates.contains(_))
                if (nextNewMoves.size != newListToTry.size) sys.error("Inconsistent number of new states to try")
                println("Skipping " + (nextMoves.size - nextNewMoves.size) + " states that were already tried")
                println("Remaining possible moves:" + nl)
                nextNewMoves.map((g: GameState) => println(g.toString + nl))
                println("Exiting")
                sys.exit(0)
              }
              newListToTry.map((h: GameHistory) => knownStates += h.state)
              playGameRec(newListToTry ++ restToTry, pastWins, knownStates, counter + 1)
            }
          }
        case _ => {
          printState(counter, statesToTry, "No moves left to try"); println()
          pastWins
        }
      }

    val game = newGame
    playGameRec(List(GameHistory(game, List[GameState]())), List[List[GameState]](), Set(game), 1)
  }

}
