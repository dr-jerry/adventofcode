package aoc2021

import scala.annotation.tailrec
import scala.io.Source


object Day01 extends App {
  println("test is " + "jeroen")
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day01.txt")).getLines().toList.map(_.toInt)
  println("size is " + array.size)
  var max = 999;
  var count = 0;
  for  (focus <- array) {
    count = count + (if (focus > max) 1 else 0)
    max = focus
  }
  println(count)

  var prevWindow = 9999
  var part2 = 0
  // second
  for (i <- 2 to array.size-1) {
    var window = array(i-2) + array(i-1) + array(i)
    println(s"$i -> $prevWindow $window")
    part2 = part2 + (if (window > prevWindow) 1 else 0)
    prevWindow = window
  }
  println("part2: " + part2)

}
