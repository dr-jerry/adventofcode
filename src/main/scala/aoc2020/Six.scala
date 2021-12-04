package aoc2020

import scala.io.Source

object Six extends App {
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("six.txt")).getLines().toList

  def process(lines: List[String], acum: String): List[String] = {
    lines match {
      case l :: tail if (l.nonEmpty) => process(tail, s"${l.trim}$acum")
      case Nil => List(acum)
      case "" :: tail => acum :: process(tail, "")
    }
  }

  val ped = process(lines, "")
  println(ped)
  val answers = process(lines, "").map(str => str.groupBy(c => c)).map(m => m.size).sum
  println(answers)

  println("part2")
  val init = "abcdefghijklmnopqrstuvwxyz".split("").toSet

  def process2(lines: List[String], acum: Set[String]): List[Set[String]] = {
    lines match {
      case l :: tail if (l.nonEmpty) => process2(tail, acum.intersect(l.split("").toSet))
      case Nil => List(acum)
      case "" :: tail => acum :: process2(tail, init)
    }
  }

  println(process2(lines, init).map(s => s.size).sum)
}
