package solitaire

object Main {

  def nl = System.getProperty("line.separator") + System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = {
    (for (i <- 1 to 5) yield Game().moveSequence(100).length).mkString(System.getProperty("line.separator"))
    
  }

}
