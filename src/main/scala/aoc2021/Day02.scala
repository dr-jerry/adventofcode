package aoc2021

import scala.io.Source


object Day02 extends App {
  def go(instructions: List[String], pos: (Int, Int)): Int = {
    instructions match {
      case head :: tail => {
        val instr = head.split(' ');
        instr(0) match {
          case "forward" => go(tail, (pos._1  + instr(1).toInt, pos._2))
          case "down" => go(tail, (pos._1, pos._2 + instr(1).toInt))
          case "up" => go(tail, (pos._1, pos._2 - instr(1).toInt))
        } }
        case _ => pos._1 * pos._2
      }
  }

  def go2(instructions: List[String], pos: (Int, Int), aim: Int): Int = {
    instructions match {
      case head :: tail => {
        val instr = head.split(' ');
        instr(0) match {
          case "forward" => go2(tail, (pos._1  + instr(1).toInt, pos._2 + aim * instr(1).toInt), aim)
          case "down" => go2(tail, pos, aim + instr(1).toInt)
          case "up" => go2(tail, pos, aim - instr(1).toInt)
        } }
      case _ => pos._1 * pos._2
    }
  }

  println("test is " + "jeroen")
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day02.txt")).getLines().toList
  println(go(array, (0,0)))
  println("part2: " + go2(array, (0,0), 0))
}
