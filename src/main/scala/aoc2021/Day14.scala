package aoc2021

import aocutils.Point

import scala.io.Source

object Day14 extends App {
  val pairPattern = "([A-Za-z])([A-Za-z]) -> ([A-Za-z])".r
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day14.txt")).getLines().toList

  def run(array: List[String]): (String, String) = {
    val (template, pairs) = array.filter(_.size != 0).partition(!_.contains("->"))
    val pair2trip = pairs.map(str => {
      val pairPattern(left, right, insert) = str
      (s"$left$right" -> s"$insert$right")
    }).toMap

    def scan(template: List[Char], accum: String): String = {
//      println(s"template $template" + accum)
//      println(s"template ${template.mkString("")}")
      template match {
        case head :: tail if (tail.size > 0) => scan(tail, accum + pair2trip(s"$head${tail(0)}"))
        case _ => accum
        }
      }

    def iterate(count: Int, template: String):String = {
      var result = scan(template.toList, template(0).toString)
      println("result " + result)
      if (count > 0) iterate(count-1, result)
      else result
    }

  val resultMap = iterate(9, template(0)).toList.groupBy(identity).view.mapValues(_.size)
    println(resultMap.toMap)
    val sortedList = resultMap.values.toList.sortWith(_>_)
    ("part1:" + (sortedList(0) -sortedList(sortedList.size-1)) , "part2")
  }

val test1 = """NNCB
              |
              |CH -> B
              |HH -> N
              |CB -> H
              |NH -> C
              |HB -> C
              |HC -> B
              |HN -> C
              |NN -> C
              |BH -> H
              |NC -> B
              |NB -> B
              |BN -> B
              |BB -> N
              |BC -> B
              |CC -> N
              |CN -> C""".stripMargin.split("\n").toList
  //println(run(test1))
  println(run(array))
}
