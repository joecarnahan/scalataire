package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats(100))

  //def main(args: Array[String]) = 
    //for (size <- List(100, 150, 200, 250, 300, -1); i <- 1 to 100)
      //println(generateStats(size) + nl + "**********************************************************" + nl + nl)

  def generateStats(size: Int) : String = {
    val wins = Game.playGame(size)
    "Got " + wins.size + " wins" + nl + nl + wins.mkString("*****************************" + nl + nl)
  }

}
