package aoc2022

import scala.io.Source

object Day04 extends App {
  val splitComma = "(.*),(.*)".r
  object Range {
    val rangePat = "(\\d+)\\-(\\d+)".r
    def apply(rangeString: String): Range = {
      val rangePat(left, right) = rangeString
      Range(BigInt(left), BigInt(right))
    }
  }
  case class Range(left: BigInt, right: BigInt) {
    def pWithin(p: BigInt): Boolean = {
      p >= left && p <= right
    }
    def within(other: Range): Boolean = {
      pWithin(other.left) && pWithin(other.right)
    }
    def overlap(other: Range): Boolean = {
      within(other) || other.within(this)
    }
    def partialOverlap(other: Range): Boolean = {
      pWithin(other.left) || pWithin(other.right)
    }
  }

  val array = Source.fromInputStream(this.getClass.getResourceAsStream( "day04.txt")).getLines().toList
  // val array = List("2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8")
  println(s"part1 ${array.filter(line => {
    val splitComma(first, second) = line
    Range(first).overlap(Range(second))
  }).size}")

  println("part2 " + array.filter(line => {
    val splitComma(first, second) = line
    Range(first).partialOverlap(Range(second)) || Range(second).partialOverlap(Range(first))
  }).size)

}
