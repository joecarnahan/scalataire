package solitaire

object Main {

  def nl = System.getProperty("line.separator")

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = 
    (1 to 10).map(_ => Game().moveSequence(10000).size.toString).mkString(nl)
    //Game().getWin.map(_.mkString(nl)).getOrElse("No solution")

}
