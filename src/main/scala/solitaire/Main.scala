package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = {
    val wins = Game.playGame
    "Got " + wins.size + " wins" + nl + nl + wins.mkString("*****************************" + nl + nl)
    //Game().getWin.map(_.mkString(nl)).getOrElse("No solution")
  }

}
