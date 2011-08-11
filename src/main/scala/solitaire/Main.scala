package solitaire

object Main {

  def nl = System.getProperty("line.separator")
  
  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = Card.allCards.map(_.toString).mkString(nl)

}
