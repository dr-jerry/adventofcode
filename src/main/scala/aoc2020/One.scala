package aoc2020

import scala.io.Source

object One extends App {
  val file = "one.txt"

  def find(list: List[Int]): Int = {
    //println("list is " + list)
    list match {
      case head :: Nil => 0
      case head :: second :: tail if head + second == 2020 => {
        println(head * second);
        head * second
      }
      case head :: second :: tail => {
        val result = find(head :: tail)
        if (result == 0)
          find(second :: tail)
        result
      }
    }
  }

  val lines = Source.fromInputStream(this.getClass.getResourceAsStream(file)).getLines().toList.map(_.trim().toInt).combinations(3)
  for (l <- lines)
    if (l.sum == 2020) println(l.product)

}
