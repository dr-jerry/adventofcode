package aoc2021

import aocutils.Point

import scala.io.Source

object Day13 extends App {
  val foldPattern = "fold along ([yx])=(\\d+)".r
  var array = Source.fromInputStream(this.getClass.getResourceAsStream( "day13.txt")).getLines()
  def run(array: List[String]): (String, String) = {
    val (folds, strpoints) = array.filter(_.size !=0).partition(_.startsWith("fold"))
    val foldPoints = folds.map(str => {val foldPattern(ax,v) = str
      if (ax == "x") Point(v.toInt, 0)
      else Point(0,v.toInt)})
    val points = strpoints.map(str => Point(str))
    def fold(pts: List[Point], seq: Seq[Point]): Set[Point] ={
//      println(s"points is $pts")
      if (pts.isEmpty) seq.toSet

      else fold(pts.tail, seq.map(pt => {
        if(pts.head.x == 0)
          if (pt.y > pts.head.y) Point(pt.x, (pts.head.y * 2 - pt.y))
          else pt
        else if (pt.x > pts.head.x) Point(pts.head.x * 2 - pt.x, pt.y)
          else pt}))
    }
    def display(pts: Seq[Point], boundary: Point): Unit = {
      var matrix :Array[Array[Char]] = Array.ofDim[Char](boundary.y, boundary.x)
      for {y <- 0 to boundary.y-1
           x <- 0 to boundary.x-1
           }(matrix(y)(x) = ' ')
        pts.foreach(pt => matrix(pt.y)(pt.x) = '#')
      println
      for {y <- 0 to boundary.y} println(matrix(y).mkString(""))
    }
    val result = fold(foldPoints, points)
    result.foreach(println(_))
    ("part1:" + fold(foldPoints.take(1), points).size,"part2:" + display(result.toSeq, Point(45,7)))
  }

val test1 = """6,10
              |0,14
              |9,10
              |0,3
              |10,4
              |4,11
              |6,0
              |6,12
              |4,1
              |0,13
              |10,12
              |3,4
              |3,0
              |8,4
              |1,10
              |2,14
              |8,10
              |9,0
              |
              |fold along y=7
              |fold along x=5""".stripMargin.split("\n").toList
  println(run(array.toList))
}
