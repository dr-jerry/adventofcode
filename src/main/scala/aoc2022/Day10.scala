package aoc2022

import aocutils.Point

import scala.io.Source

object Day10 extends App {
  val test = Source.fromInputStream(this.getClass.getResourceAsStream("day10.txt")).getLines().toList
  //val real = aocutils.getFromAOC(2022,10).split("\n").toList
  def part1(cmds: List[String]): Unit = {

  val alues =  cmds.foldLeft((List(1), 0))((tup, s) => {
    val pat = "addx (-?\\d+)".r
    val newValue = tup._1.last + tup._2
    if (s == "noop") {
      (tup._1 :+ newValue, 0)
    } else {
      val pat(end) = s
      val newValue = tup._1.last + tup._2
      (tup._1 :++ List(newValue, newValue), end.toInt)
    }})._1
    val indices = (0 to 5).map(i => i*40+20)
    indices.foreach(i => println(s"$i * ${alues(i)} = ${i * alues(i)}"))
    println(s"${indices.map(i => alues(i) *i).sum}")
    val crt = (1 to 240).map(i =>
        if (i % 40 >= alues(i) && i % 40 <= alues(i)+2) '#' else ' ')
    println(crt.grouped(40).map(_.mkString).mkString("\n"))

  }

  part1(test)



}
