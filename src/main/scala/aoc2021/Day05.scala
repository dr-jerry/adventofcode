package aoc2021

import aocutils.{Line, Point}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day05 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day05.txt")).getLines().toList

  val pattern = "^(\\d+),(\\d+) -> (\\d+),(\\d+)$".r

  val lines = array.map(str => {val pattern(x1,y1,x2,y2) = str.trim
     Line(Point(x1, y1), Point(x2,y2))})
    .filter(l => l.s.x == l.e.x || l.s.y == l.e.y
       || (Math.abs(l.s.x - l.e.x) == Math.abs(l.s.y - l.e.y))) // extra for part2
     //Line(Coor(x1, y1), Coor(x2, y2))) = str.trim})
  val result = ListBuffer[Point]()
  lines.foreach(l => result ++= l.s.inBetween(l.e, None, List[Point]()))
  val lr : Iterable[Int] = result.groupBy(identity).mapValues(_.size).toMap.values.filter(_>=2)
  println(s" part a : ${lr.size}")
}
