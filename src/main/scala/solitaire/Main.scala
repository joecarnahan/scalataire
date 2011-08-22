package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = 
    Game().moveSequence(10000).size.toString
    //Game().getWin.map(_.mkString(nl)).getOrElse("No solution")

}
