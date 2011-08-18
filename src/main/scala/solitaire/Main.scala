package solitaire

object Main {

  def nl = System.getProperty("line.separator") + System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = {
    val game = Game()
    "Initial game:" + nl + game.toString + nl + "Next moves:" + nl + game.nextMoves.mkString(nl)
  }

}
