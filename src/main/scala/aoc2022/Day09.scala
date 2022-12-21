package aoc2022

import aocutils.{Point, points2D}

import scala.io.Source

object Day09 extends App {
  val test = Source.fromInputStream(this.getClass.getResourceAsStream("day09.txt")).getLines().toList
  val real = aocutils.getFromAOC(2022,9).split("\n").toList
  def pull(head: Point, tail: Point, debug: Boolean= false): Point = {
    val result =
      if (head.adjacent(tail)) Point(0,0)
      else
        head - tail match {
          case Point(0, 0) => Point(0, 0)
          case Point(0, y) => Point(0, if (y > 0) y - 1 else y + 1)
          case Point(x, 0) => Point(if (x > 0) x - 1 else x + 1, 0)
          case Point(x, y) => if (math.abs(x) + math.abs(y) >= 4) Point(if (x > 0) x - 1 else x + 1, if (y > 0) y - 1 else y + 1)
            else if (math.abs(x) > math.abs(y)) Point(if (x > 0) x - 1 else x + 1, y)
            else  Point(x, if (y > 0) y - 1 else y + 1)
        }
//      if (debug) println(s"pull $head on $tail gives : $result")
     result
    }

  val cmd2Delta = Map("U" -> Point(0,-1), "D" -> Point(0,1), "L" -> Point(-1,0), "R" -> Point(1,0))
  val pat = "([A-Z]) (\\d+)".r
  def part1 (lines: List[String]) = {
    var head = Point(0,0);
    var tail = Point(0,0);
    var visited = Set[Point]()
    lines.map(l => { val pat(cmd, dist) = l;
      val delta = cmd2Delta(cmd)
      for (i <- 1 to dist.toInt) {
        head = head + delta
        val d = pull(head, tail); tail = tail + d;
         visited += tail
      }
    })
    println(visited.size)
  }

  def part2 (lines: List[String]) = {
    var knots = (1 to 10).map(_ => Point(0,0)).toVector
    var visited = Set.empty[Point]
    lines.map(l => {
      val pat(cmd, dist) = l;
      val delta = cmd2Delta(cmd)
      val head = knots.head
      for (i <- 1 to dist.toInt) {
        val head = knots.head
        val debug = cmd == "U" && i == 4
        var count = 1
        knots = knots.tail.foldLeft(Vector(knots.head + delta))((result, p) => {
//          if (debug) count += 1
//          if (count >= 5)
// //           println(s"count: $count p: $p result $result delta: ${pull(result.last, p)} new:${p + pull(result.last, p)}" )
          val newTailKnot = p + pull(result.last, p, debug)
          //visited += newTailKnot
          result.appended(newTailKnot)
        })
        visited += knots.last
      }
      //println(s"${points2D(knots)}")
    })
    println(visited.size)

  }
  val head = Point(2,-2)
  val tails = List((1,1), (5,1), (1,2), (4,2), (5,3), (1,3),(4,3),(1,0),(0,0))
  tails.map(tup => Point(tup._1, tup._2)).foreach(pt => {val d = pull(head, pt);println(s"pull ($head, $pt) => $d; ${d + pt}")})
    part1(real)
  part2(real)





}
