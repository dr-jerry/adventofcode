package aoc2022

import scala.io.Source

object Day01 extends App {
  var array: List[String] = Source.fromInputStream(this.getClass.getResourceAsStream("day01.txt")).getLines().toList :+ ""
  println("size is " + array.size)
  var result = array.foldLeft(0,0)((tuple, s) => {
    if (s == "") {
      if (tuple._2 > tuple._1) (tuple._2, 0)
      else (tuple._1, 0)
    } else (tuple._1, tuple._2 + s.toInt)
  })
  println(s"part1 is ${result._1}" )
  println("part2 is " + array.foldLeft(List.empty[Int], 0)((tuple, s) => {
    if (s == "")
      (tuple._1 :+ tuple._2, 0)
    else (tuple._1, tuple._2 + s.toInt)
  })._1.sorted.reverse.take(3).sum)
}
