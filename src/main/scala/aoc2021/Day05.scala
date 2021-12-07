package aoc2021

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Point {
  def apply(xStr: String, yStr: String): Point = {
    Point(xStr.toInt, yStr.toInt)
  }
}

case class Line(s: Point, e: Point)
case class Point(val x: Int, y: Int) {
  def +(coor: Point): Point = {
    Point(x + coor.x, y + coor.y);
  }
  def -(coor: Point): Point = {
//    println(s"minning this $this, that: $coor")
    Point(x - coor.x, y - coor.y)
  }

  def inBetween(coor: Point, delta: Option[Point], result: Seq[Point]): Seq[Point] = {
//    println(s"coor: $coor , this: $this")
    val d : Point = delta.fold{val d1 = this - coor
      d1 match {
        case Point(x, y) if x < 0 && y == 0 => Point(-1,0)
        case Point(x, y) if y < 0 && x == 0 => Point(0, -1)
        case Point(x, y) if x > 0 && y == 0 => Point(1,0)
        case Point(x, y) if y > 0 && x == 0 => Point(0, 1)
        // extra for part2
        case Point(x, y) if x < 0 && y < 0 => Point(-1,-1)
        case Point(x, y) if y < 0 && x > 0 => Point(1, -1)
        case Point(x, y) if x > 0 && y > 0 => Point(1,1)
        case Point(x, y) if y > 0 && x < 0 => Point(-1, 1)
      }
    }(c => c)
    if (coor == this) result :+ coor
    else {
      inBetween(coor + d, Some(d), result :+ coor )
    }
  }


}
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
  val lr : Iterable[Int] = result.groupBy(identity).view.mapValues(_.size).toMap.values.filter(_>=2)
  println(s" part a : ${lr.size}")
}
