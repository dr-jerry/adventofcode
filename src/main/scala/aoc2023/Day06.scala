package aoc2023

import scala.io.Source

object Day06 extends App {

  //val real = Source.fromInputStream(this.getClass.getResourceAsStream("day06.txt")).getLines().toList

  val real = aocutils.getFromAOC(2023, 6).split("\n").toList
  val s"Time: $time" = real(0)
  val s"Distance: $dist" = real(1)
  val res = time.trim.split(" +").zip(dist.trim.split(" +")).map(t => (0 to t._1.toInt)
    .filter(r => r * (t._1.toInt - r) > t._2.toLong).size.toLong).product
  println(s"part1: $res")

  val b = time.trim.replaceAll(" +", "").toLong
  val c = dist.trim.replaceAll(" +", "").toLong
  val ac = Math.sqrt(b*b-4*c)/2L
  val total = (b-Math.sqrt(b*b-4*c))/2L
  println(s"part2: totaltime = $b/2 => ${b/2L - ac} - ${Math.floor(b/2L +ac) - Math.floor(b/2L- ac)}")

 }
