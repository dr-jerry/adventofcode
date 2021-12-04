package aoc2020

import scala.collection.mutable
import scala.io.Source

object SevenB extends App {

  case class Settimes(val times: Int, id: String, val set: Set[Settimes])

  val pattern = "(\\d*) ?(.*)bags?.?".r

  def sanitize(str: String): (Int, String) = {
    val pattern(count, mid) = str.trim
    (try {
      count.toInt
    } catch {
      case ex: Throwable => 0
    }, mid.trim)
  }

  //
  //  def countBags(id: String): BigInt {
  //    ruleMap.getOrElse()
  //  }
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/seven.txt")).getLines().toList

  val ruleMap = mutable.Map(lines.map(l => {
    val a = l.split(" contain ")
    val set = a(1).split(',').map(str => {
      val t = sanitize(str)
      s"(${t._1} * '${t._2}')"
    }).toSet
    sanitize(a(0))._2 -> set
  }): _*)
  //  println(s"bag: ${bags("faded blue")}")
  //println(bags)
  println(ruleMap("shiny gold"))


  val tPattern = "\\((\\d+) \\* '(.*)'\\)".r

  def setExpand(set: Set[String]): Set[Settimes] = {
    set.map(bag => {
      val tPattern(count, id) = bag
      expandBag(id, count.toInt)
    })
  }

  def expandBag(id: String, count: Int): Settimes = {
    Settimes(count, id, setExpand(ruleMap.getOrElse(id, Set())))
  }

  val result = expandBag("shiny gold", 1)
  println(result)

  def calculate(st: Settimes): BigInt = {
    st match {
      case Settimes(1, "no other", _) => {
        println(1); 1
      }
      case Settimes(c, _, set) => c + c * set.map(st => calculate(st)).sum
    }
  }

  println(calculate(result) - 1)
}
