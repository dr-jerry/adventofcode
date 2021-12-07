package aoc2021

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day07 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day07.txt")).getLines().toList(0)
    .split(",").map(_.toInt)
  //println(array(0))
  val grouped = array.groupBy(identity).view.mapValues(l => l.size).toMap
  println(s" part a : ${grouped}")

  var min = 1000000
  var min2 = BigInt(100000000)
  for(i <- 300 to 700) {
    var result = 0
    var result2 = BigInt(0);
    for(key <- grouped.keys) {
      val diff = Math.abs(i-key)
      result = result + diff * grouped(key)
      result2 = result2 + diff * (diff+1) /2 * grouped(key)
    }
    if (result < min) {
      println(s"[1] $i -> $result")
      min = result
    }
    if (result2 < min2) {
      println(s"[2] $i -> $result2")
      min2 = result2
    }

  }
}
