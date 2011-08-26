package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = for (i <- 1 to 100) println(generateStats)

  def generateStats : String = {
    val wins = Game.playGame
    "Got " + wins.size + " wins" + nl + nl + wins.mkString("*****************************" + nl + nl)
  }

}
