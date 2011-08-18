package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = {
    var game = Game()
    (for (i <- 1 to 20) yield {
      game = game.nextMoves.head
      GameState(game.deck, game.stack, game.suits, game.piles)
    }).mkString(nl + nl)
  //  "Initial game:" + nl + game.toString + nl + "Next moves:" + nl + game.nextMoves.mkString(nl)
  }

}
