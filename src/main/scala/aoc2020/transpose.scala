package aoc2020

object transpose extends App {
  val ar = Array(Array("2", "1", "2", "3"), Array("5", "4", "3", "2"), Array("8", "7", "6", "4"), Array("1", "1", "1", "5"))
  ar.transpose.foreach(s => println(s.mkString(",")))
}
