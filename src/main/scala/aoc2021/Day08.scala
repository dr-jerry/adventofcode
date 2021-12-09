package aoc2021

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day08 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day08.txt")).getLines().toList
  val pattern = "(.*) \\| (.*)".r
  val uniqueLengths = Set(3, 2, 4, 7)
  var part1 = array.map(str => {
    val pattern(ten, four) = str.trim
    four.split(" ").filter(code =>  uniqueLengths.contains(code.length)).size
  }).sum
  println(s"part1: $part1"

  )
  def intersect(cand: String, mask: String):Boolean = {
    mask.split("").toSet.subsetOf(cand.split("").toSet)
  }

  def decode(ten: String): Map[String, Int] = {
    val numbers = ListBuffer(ten.split(" ").map(str => str.split("").sorted.mkString("")):_*)

    val ds = ArrayBuffer[String]("", "", "", "", "", "", "", "","", "","")
    //println(numbers.filter(_.size == 2)(0))
    ds(1) = numbers.filter(_.size == 2)(0).split("").sorted.mkString("")
    ds(4) = numbers.filter(_.size == 4)(0).split("").sorted.mkString("")
    ds(7) = numbers.filter(_.size == 3)(0).split("").sorted.mkString("")
    ds(8) = numbers.filter(_.size == 7)(0).split("").sorted.mkString("")
    ds(9) = numbers.filter(c => c.size == 6 && intersect(c, ds(4)))(0)
    numbers --= List(ds(9), ds(1), ds(4), ds(7), ds(8))
    ds(0) = numbers.filter(c => c.size == 6 && intersect(c, ds(1)))(0)
    numbers -= ds(0)
    ds(3) = numbers.filter(c => c.size == 5 && intersect(c, ds(1)))(0)
    //println(s"3: ${ds(3)}")
    ds(6) = numbers.filter(c => c.size == 6)(0)
    numbers --= List(ds(3), ds(6))
    //println(s"numbers: $numbers")
    //println(s"ds: $ds numbers $numbers intersect: ${intersect(ds(9),numbers(1))}")
    ds(5) = numbers.filter(c => c.size == 5 && intersect(ds(9), c))(0)
    numbers -= ds(5)
    ds(2) = numbers(0)
    //println(s"32: ${ds(3)}")

    //println(s"ds is $ds")
    ds.zipWithIndex.toMap
  }

  var part2 = array.map(str => {
    val pattern(ten, four) = str.trim
    val decoders = decode(ten)
    //println(decoders)
    four.split(" ")
      .map(str => { val index = str.split("").sorted.mkString("")
        decoders(index)}).mkString("").toInt
  }).sum
  println(s"part2: $part2")
}
