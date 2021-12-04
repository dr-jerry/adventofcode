package aoc2020

import scala.collection.mutable
import scala.io.Source

object Eight extends App {

  case class Op(val instr: String, val arg: Int)

  case class Status(val acc: Int, val index: Int)

  val code = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/eight.txt")).getLines().toList
    .map(line => {
      val a = line.split(" +?")
      Op(a(0), a(1).toInt)
    }).toArray

  def run(): Boolean = {
    val visited: mutable.Set[Int] = mutable.Set()
    var index = 0
    var acc = 0
    while (!visited.contains(index) && index < code.length) {
      visited.add(index)
      acc = code(index) match {
        case (Op("nop", _)) => {
          index += 1
          acc
        }
        case Op("acc", arg) => {
          index += 1
          acc + arg
        }
        case Op("jmp", arg) => {
          index += arg
          acc
        }
      }
    }
    if (index == code.length) {
      println(s"found $index, ACC $acc")
      return true
    }
    false
  }

  (0 to code.length).foreach(i => code(i) match {
    case (Op("jmp", arg)) => {
      code(i) = Op("nop", arg)
      if (run()) println(s"jmp2nop at $i")
      code(i) = Op("jmp", arg)
    }
    case (Op("nop", arg)) => {
      code(i) = Op("jmp", arg)
      if (run()) println(s"nop2jmp at $i")
      code(i) = Op("nop", arg)
    }
    case _ => ()
  })
}
