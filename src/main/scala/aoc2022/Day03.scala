package aoc2022

import scala.io.Source

object Day03 extends App {
  println(s"classname: ${this.getClass.getName}")
  def ch2res(c: Char): Int = {
    val i = c.toInt
    if (i > 96) i - 96
    else i - 65 + 27
  }

  var array: List[String] = Source.fromInputStream(this.getClass.getResourceAsStream("day03.txt")).getLines().toList
  val grouped = array.grouped(3)
  println(grouped.take(5).toList)
  val result = array.map(line => {
    var t = (line.substring(0,line.length/2), line.substring(line.length/2))
    ch2res(t._1.toCharArray.toList.intersect(t._2.toCharArray)(0))

  }).sum
  println("part2")
    val part2 = array.foldLeft((0, List.empty[String]))((acc, line) =>  {
      if (acc._2.size == 2) {
        var three = acc._2
        val r = ch2res(three(0).toCharArray.toList
          .intersect(three(1).toCharArray.toList
            .intersect(line.toCharArray.toList))(0))
        (acc._1 + r, List.empty[String])
      } else {

        (acc._1, acc._2 :+ line)
      }
  })
  println(s"part1 $result")
  println(s"part2 ${part2._1}")


}
