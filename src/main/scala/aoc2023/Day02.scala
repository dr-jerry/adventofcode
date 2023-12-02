package aoc2023
import scala.io.Source

object Day02 extends App {

  val nrs = Map[String,Int]("1"->1, "2"->2, "3"->3, "4"->4, "5"->5, "6"->6, "7"->7, "8"->8, "9"->9
    , "one"->1, "two"->2, "three"->3, "four"->4, "five"->5, "six"->6, "seven"->7, "eight"->8,"nine"->9)
  def part1(list: List[String]): Long = {
    list.foreach(s => {
      val "Game $id: $contents" = s
      println(id)
  })
    0L
  }


  val real = Source.fromInputStream(this.getClass.getResourceAsStream("day02.txt")).getLines().toList
  part1(real)
  //val real = aocutils.getFromAOC(2023, 1).split("\n").toList
  //println(s"outcome = ${real.map(l => toNum(l)).sum}")
  //println(s"2 outcome = ${part2(real)}")

}
