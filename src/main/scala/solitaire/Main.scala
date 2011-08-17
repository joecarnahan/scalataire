package solitaire

object Main {

  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = Game().toString

}
