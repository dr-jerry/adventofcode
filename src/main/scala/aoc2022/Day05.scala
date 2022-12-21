package aoc2022

import scala.io.Source

object Day05 extends App {
  val array = Source.fromInputStream(this.getClass.getResourceAsStream( "day05.txt")).getLines().toList
  //val array = aocutils.getFromAOC(2022,5).split("\n")
  println(s"size is ${array.size}")
  // first get out stacks.
  val staks = array.filter(line => line.indexOf("[") != -1).reverse
  // its reversed so the longest line is at top
  val max = staks.map(line => line.size).max
  // make all rows are equal length, transpose, select the propriate lines
  val stacks = staks.map(l => l.padTo(max, ' ')).transpose.tail.sliding(1,4)
    .map(_.head.reverse.mkString.trim).toArray

  val parsePat = "move (\\d+) from (\\d+) to (\\d+)".r
  val ops = array.filter(line => line.startsWith("move"))
  ops.map(line => {
    val parsePat(sz, ori, ds) = line
    val (size, orig, dest) = (sz.toInt, ori.toInt -1, ds.toInt -1)
    // part1: val toMove = stacks(orig.toInt - 1).substring(0,size.toInt).reverse
    val toMove = stacks(orig).substring(0,size) // part2 (reverse removed)
    stacks(orig) = stacks(orig).substring(size)
    stacks(dest) = toMove + stacks(dest)
  })
  println(s"part1: ${stacks.toList.map(l => l.substring(0,1)).mkString}")

//    .map(ca => {println(s"ca is $ca");ca.filter(c => {println("c is " + c); c != ' '}).toString}).foreach(println(_))


}
