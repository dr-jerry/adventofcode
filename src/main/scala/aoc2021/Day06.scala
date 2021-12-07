package aoc2021

import aocutils.{Line, Point}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day06 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day06.txt")).getLines().toList(0)
    .split(",").map(_.toInt)
  //println(array(0))
  val grouped = array.groupBy(identity).view.mapValues(l => BigInt(l.size)).toMap
  println(s" part a : ${grouped}")
  var ocean = ArrayBuffer(array:_*)
  def iterate(playGround: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    //println(s"iterate ${playGround} ")
    var toAppend = ArrayBuffer[Int]()
    for (i <- 0 to playGround.size-1) {
      if (playGround(i) == 0 ) {
        toAppend += 8
        playGround(i) = 6
      } else {
        playGround(i) = playGround(i)-1
      }
    }
    //println(s"to Append ${toAppend.mkString(",")}")
    toAppend
  }
  for (x <- 0 to 79) {
    ocean ++= iterate(ocean)
  }
  println(s"size ${ocean.size}")

  // part2
  def iterate2(index: Int, population: Map[Int, BigInt]): Map[Int, BigInt] = {
    //println(s"$index + $population")
    def subIterate(index: Int, map: Map[Int, BigInt]): Map[Int, BigInt] = {
      //println(s"subiterate $index, $map")
      index match {
        case 9 => map - 9  // remove dummy index 9
        case _ => subIterate(index+1, map + ((index) -> map.getOrElse((index+1), BigInt(0))))
      }
    }
    index match {
      case 0 => population
      case _ => iterate2(index-1, subIterate(0, population ++ List(9 -> population.getOrElse(0, BigInt(0))
      ,7-> (population.getOrElse(7, BigInt(0))
        + population.getOrElse(0, BigInt(0))))))}
  }
  //val part2result = iterate2(256, grouped)
  println(s"part2: ${iterate2(256, grouped).values.sum}")
}
