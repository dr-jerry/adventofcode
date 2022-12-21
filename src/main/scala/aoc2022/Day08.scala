package aoc2022

import aocutils.Point

import scala.io.Source
/*
Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.

A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:

The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
The top-middle 5 is visible from the top and right.
The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
The left-middle 5 is visible, but only from the right.
The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
The right-middle 3 is visible from the right.
In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.

Consider your map; how many trees are visible from outside the grid?
 */

object Day08 extends App {
  case class Tree(var height: Int, var seen: Int)

  val test = Source.fromInputStream(this.getClass.getResourceAsStream( "day08.txt")).getLines().toList
  val real = aocutils.getFromAOC(2022,8).split("\n").toList
  //println(s"part1: ${stacks.toList.map(l => l.substring(0,1)).mkString}")
  def findAsc(line: List[Tree]): List[Tree] = {
      line.foldLeft((-1, List.empty[Tree]))((acc, tree) => {
        val (max, list) = acc
        if (tree.height > max) (tree.height, list :+ tree.copy(seen = tree.seen+1))
        else (acc._1, acc._2 :+ tree)})._2
  }

  def bothEnds(line: List[Tree]): List[Tree] = {
    findAsc(findAsc(line).reverse)
  }

  def part1(lines: List[String]): Int = {
    val forest = lines.map(str => str.map(ch => Tree(ch.toString.toInt, 0)))
    forest.map(l => bothEnds(l.toList)).transpose.map(l => bothEnds(l)).map(l => l.filter(_.seen >= 1).size).sum
  }
//  println(part1(test))
//  println(s"real: ${part1(real)}")

  def part2(lines: List[String]): BigInt = {
    def scan(height: Int, focus: Point, delta: Point, boundary: Point, woud: Vector[Vector[Int]], count: Int): BigInt = {
      if (!focus.in(boundary)) count else {
        if (woud(focus.x)(focus.y) < height) scan(height, focus + delta, delta, boundary, woud, count + 1)
        else {
          BigInt(count+1)
        }
      }
    }
    val deltas = List((-1,0),(1,0),(0,-1),(0,1)).map(t => Point(t._1, t._2))
    val forest = lines.map(str => str.map(_ - '0').toVector).toVector
    val boundary = Point(forest.size-1, forest(0).size-1)
    var result = BigInt(0);
    for ( x <- 0 to forest.size-1) {
      for (y <- 0 to forest(x).size-1) {
        val fc = Point(x,y);
        val calc = deltas.map(c => scan(forest(x)(y), fc+c, c, boundary, forest, 0)).product
        if (calc > result) result = calc
      }
    }
    result
  }
  println(part2(real))
}
