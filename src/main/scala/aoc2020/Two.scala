package aoc2020

import scala.io.Source

object Two extends App {
  val file = "two.txt"

  case class Entry(val min: Int, val max: Int, val char: Char, password: String) {
    def isValid(): Boolean = {
      val count = password.count(_ == char)
      return count >= min && count <= max
    }

    def isValid2(): Boolean = {
      List(min, max).map(_ - 1).filter(i => password.charAt(i) == char).size == 1
    }
  }

  def line2Entry(line: String): Entry = {
    line.trim.split("[\\-: ]") match {
      case Array(min, max, kar, sp, password) => Entry(min.toInt, max.toInt, kar.charAt(0), password)
      case _ => Entry(0, 0, 'z', "Fuck")
    }
  }


  println(line2Entry("14-15 g: ggrbggglmktgvjg"))
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream(file)).getLines().toList.map(_.trim())
  println(s"count is ${lines.map(line2Entry(_)).filter(_.isValid()).size}")
  println(s"count2 is ${lines.map(line2Entry(_)).filter(_.isValid2()).size}")

}
