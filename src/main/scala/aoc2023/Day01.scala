package aoc2023

import scala.io.Source


object Day01 extends App {

  val nrs = Map[String,Int]("1"->1, "2"->2, "3"->3, "4"->4, "5"->5, "6"->6, "7"->7, "8"->8, "9"->9
    , "one"->1, "two"->2, "three"->3, "four"->4, "five"->5, "six"->6, "seven"->7, "eight"->8,"nine"->9)
  def part2(list: List[String]): Long = {
    def firstNLast(line: String): Int  = {
      val found : ((String, Int), (String, Int)) = nrs.keys.foldLeft((("",1000),("",-1))){ (subresult, s) => {
        val sIndex  = (line + s).indexOf(s)
        val eIndex = (s + line).lastIndexOf(s)
        (if (subresult._1._2 > sIndex) (s, sIndex) else subresult._1
          , if (subresult._2._2 < eIndex) (s,eIndex) else subresult._2)
       }}
      nrs.get(found._1._1).get * 10 + nrs.get(found._2._1).get
    }
    list.map(firstNLast(_)).sum
  }

  def toNum(line: String): Int = {
    val firstNum = ".*?(\\d).*".r
    val firstNum(num) = line
    val firstNum(num2) = line.reverse
    println(s"$line -> $num$num2")
    s"$num$num2".toInt
  }

  //val real = Source.fromInputStream(this.getClass.getResourceAsStream("day01-2.txt")).getLines().toList
  val real = aocutils.getFromAOC(2023, 1).split("\n").toList
  //println(s"outcome = ${real.map(l => toNum(l)).sum}")
  println(s"2 outcome = ${part2(real)}")

}
