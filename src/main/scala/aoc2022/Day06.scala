package aoc2022

import scala.io.Source

object Day06 extends App {
  val array = Source.fromInputStream(this.getClass.getResourceAsStream( "day06.txt")).getLines().toList
  val real = aocutils.getFromAOC(2022,6).split("\n")
  //println(s"part1: ${stacks.toList.map(l => l.substring(0,1)).mkString}")
  def part1(line: String, size: Int): Int = {
    line.toCharArray.sliding(size,1).zipWithIndex.find(tup => {
      tup._1.groupBy(identity).size == size}).fold(-1)(t => t._2+size)
  }


//    .map(ca => {println(s"ca is $ca");ca.filter(c => {println("c is " + c); c != ' '}).toString}).foreach(println(_))
  array.foreach(l => println(part1(l,14)))
  println("p2 real")
  real.foreach(l => println(part1(l,14)))
}
