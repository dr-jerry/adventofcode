package aoc2022

import aocutils.{Complex, Point}

import scala.io.Source


object Day21 extends App {

  val test = Source.fromInputStream(this.getClass.getResourceAsStream("day21r.txt")).getLines().toList
  def toMap(lines: Seq[String]): Map[String, String] = {
    lines.map(str => {
      val Array(key, keyval) = str.split(": ")
      (key -> keyval)
    }).toMap
  }
  val linePat = """(.*): (.*) ([\-\+\*/]) (.*)""".r
  val numberPat = """(-?\d+)""".r
  val exprPat = """(.*) ([\-\+\*/]) (.*)""".r


  def part1(yells: Map[String, String], key: String): Long = {
    yells(key) match {
      case numberPat(nr) => nr.toLong
      case exprPat(v1, op, v2) => {
        op match {
          case "+" => part1(yells, v1) + part1(yells, v2)
          case "-" => part1(yells, v1) - part1(yells, v2)
          case "*" => part1(yells, v1) * part1(yells, v2)
          case "/" => part1(yells, v1) / part1(yells, v2)
        }
      }
    }
  }

  def part2(yells: Map[String, String], key: String): Unit = {
    var yels = yells
    val exprPat(v1, op, v2) = yells(key)
    // 3360562285171
    (3360561285170L to 3360561285180L by 1L).toList.foreach(nr => {
      yels += ("humn" -> nr.toString)
      val p1 = part1(yels, v1)
      val p2 = part1(yels, v2)
      println(s"${nr}: $p1 - $p2: ${p1 - p2}")
    })
  }

  def compute(yells: Map[String, String], key: String, init: Complex): Complex = {
    val result = yells(key) match {
      case numberPat(nr) if key != "humn" => Complex(nr.toLong, 0)
      case numberPat(nr) if key == "humn" => {println(s"found & init");init}
      case exprPat(lhs, op, rhs) => {
        op match {
          case "+" => compute(yells, lhs, init) + compute(yells, rhs, init)
          case "-" => compute(yells, lhs, init) - compute(yells, rhs, init)
          case "*" => compute(yells, lhs, init) * compute(yells, rhs, init)
          case "/" => compute(yells, lhs, init) / compute(yells, rhs, init)
        }
      }
    }
    //println("result is" + result)
    result
  }

  // The list of solutions sorted in merits of this part2 challenge
  // 1) Bisecting
  // 2) lineair regression
  // 3) reversing the operation.
  // 4) using complex numbers (javascript solution https://www.honingjs.com/challenges/adventofcode/2022/day-21)
  // Complex numbers need to be in floating format (Double) which probably also holds true for option 3.

  def part2Complex(yells: Map[String, String], key: String): Unit = {
    val exprPat(lhs, op, rhs) = yells(key)
    val List(l,r) = List(lhs, rhs).map(side => compute(yells, side, Complex(0,1)))
    val pair = if (r.isImag) (r, l) else (l,r)
    println(s"pair is $pair")
    println(s" complex: ${(pair._2.real - pair._1.real)/pair._1.imag}")
    // And somehow this feels like lineair regression..
  }

  println(s"part1 is ${part1(toMap(test), "root")}")
  part2(toMap(test), "root")
  part2Complex(toMap(test), "root")

}
