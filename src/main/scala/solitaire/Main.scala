package solitaire

import scala.util.Random

object Main {

  def nl = System.getProperty("line.separator")
  
  def main(args: Array[String]) = println(generateStats)

  def generateStats : String = Deck.shuffle(new Random).toString

}
