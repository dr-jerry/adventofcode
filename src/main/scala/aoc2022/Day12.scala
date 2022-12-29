package aoc2022

import aocutils.Point

import scala.collection.mutable
import scala.io.Source

object Day12 extends App {
  val test = Source.fromInputStream(this.getClass.getResourceAsStream("day12r.txt")).getLines().toList
  //val real = aocutils.getFromAOC(2022,12).split("\n").toList

  var start, end = Point(1,1)
  def parse(lines: Seq[String]): Map[Point, Char] = {
    val points = for (y <- lines.indices; x <- lines(0).indices) yield (Point(x, y) -> lines(y)(x))
    points.toMap
  }

  def parseToLines(lines: Seq[String]): Seq[Seq[Int]] =
    lines.map(s => s.toCharArray.map(ch => ch match {
      case 'S' => 0
      case 'E' => 25
      case _ => ch - 'a'
    }))

  def scan(lines: Seq[String], focus: Char): Option[Point] = {
    lines.indices.toList.find(l => lines(l).indexOf(focus) != -1).fold(Option.empty[Point])(i => Some(Point(lines(i).indexOf(focus), i)))
  }

  val directions = List(Point(1,0), Point(0,-1), Point(0,1), Point(-1, 0))

  def walk(endPredicate: (Point) => Boolean, nextPointPredicate: (Point, Point) => Boolean, todo: Seq[Point], cost: Map[Point, Int]) : Int = {
    //println(s"head: ${todo.head} cost $cost todo: $todo")
    if (endPredicate(todo.head)) cost(todo.head)
    else if (todo.isEmpty) -1
    else {
      val newTodos = directions.map(delta => delta + todo.head).filter(newIndex => nextPointPredicate(todo.head, newIndex)
          && !cost.contains(newIndex))
      walk(endPredicate, nextPointPredicate, todo.tail :++ newTodos, cost ++ newTodos.map(pt => (pt -> (cost(todo.head) +1))).toMap)
    }
  }


   def part1 (lines: List[String]) = {
     val grid = parseToLines(lines)
     val start = scan(lines, 'S').getOrElse(Point(0, 0))
     def endPointPred(grid: Seq[Seq[Int]])(focus: Point) = {focus == scan(lines, 'E').getOrElse(Point(0, 0))}
     def nextPointPred(grid: Seq[Seq[Int]])(cur: Point, newPoint: Point) = {
       //println(s"oint $cur boundary: ${Point(grid.head.size-1, grid.size-1)}  boolean: ${cur.in(Point(grid.head.size-1, grid.size-1))}")
       newPoint.in(Point(grid.head.size-1, grid.size-1)) && grid(newPoint.y)(newPoint.x) - grid(cur.y)(cur.x) <= 1}
     val cost = walk(endPointPred(grid) _
       , nextPointPred(grid) _
       , List(scan(lines, 'S').getOrElse(Point(0, 0)))
       , Map(start -> 0))
     println(s"1) cost is $cost")
   }

  def part2(lines: List[String]) = {
    val grid = parseToLines(lines)

    def endPointPred(grid: Seq[Seq[Int]])(focus: Point) = {
      grid(focus.y)(focus.x) == 0
    }

    def nextPointPred(grid: Seq[Seq[Int]])(cur: Point, newPoint: Point) = {
      newPoint.in(Point(grid.head.size-1, grid.size-1)) && grid(cur.y)(cur.x) - grid(newPoint.y)(newPoint.x) <= 1
    }

    val start = scan(lines, 'E').getOrElse(Point(0, 0))
    val cost = walk(endPointPred(grid) _ , nextPointPred(grid) _, List(start)
      , Map(start -> 0))
    println(s"2) cost is $cost")
  }
  //println(s"in test ${Point(0,5).in}")
  part1(test)
  part2(test)
}
