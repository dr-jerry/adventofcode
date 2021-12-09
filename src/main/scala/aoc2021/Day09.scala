package aoc2021

import aoc2021.Day09.boundary

import scala.io.Source
import aocutils._

import scala.collection.mutable.ListBuffer

object Day09 extends App {


  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day09.txt")).getLines()
    .map(_.split("").map(_.toInt)).toArray

  val boundary = Point(array(0).size - 1, array.size - 1)
  val deltas = List(Point(1, 0), Point(-1, 0), Point(0, -1), Point(0, 1))


  def scan(lastDepth:Int, p: Point, basin: Set[Point]): Set[Point] = {
    if (basin.contains(p) || !boundary.of(p) || array(p.y)(p.x) == 9 || array(p.y)(p.x) < lastDepth) basin
    else
      deltas.flatMap(d => scan(array(p.y)(p.x), p + d, basin+p)).toSet
  }

  var result=0
  var lengths = ListBuffer[Int]()
  //println(s"bound $boundary")
  for (x <- 0 to boundary.x;y <- 0 to boundary.y) {
    var f = array(y)(x).toInt
    if ((x <= 0 || array(y)(x-1).toInt > f) && (x >= boundary.x ||array(y)(x+1).toInt > f)
      && (y <= 0 || array(y-1)(x).toInt > f) && (y >= boundary.y || array(y+1)(x).toInt > f)) {
      result = result + f.toInt + 1;
      val basin = scan(f, Point(x,y), Set[Point]())
      lengths += basin.size
    }
  }
  println(s"part 1: $result")
  println(s"part2 ${lengths.sorted.takeRight(3).product}")
}
