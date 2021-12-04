package aoc2020

import scala.collection.mutable
import scala.io.Source

object Seven extends App {
  val pattern = "\\d* ?(.*)bags?.?".r

  def sanitize(str: String): String = {
    val pattern(mid) = str.trim
    mid.trim
  }

  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/seven.txt")).getLines().toList

  val bags = mutable.Map(lines.map(l => {
    val a = l.split(" contain ")
    val set = a(1).split(',').map(str => sanitize(str)).toSet
    sanitize(a(0)) -> set
  }): _*)
  //  println(s"bag: ${bags("faded blue")}")
  println(bags)


  def expandAndFind(set: Set[String]): Boolean = {
    if (set.contains("shiny gold"))
      true
    else {
      val expanded = set.flatMap(bag => bags.getOrElse(bag, Set()))
      if (expanded.diff(set).equals(Set()))
        false
      else
        expandAndFind(expanded)
    }
  }

  println(bags.keys.toList.filter(key => expandAndFind(bags(key))).size)

  // part2

}
