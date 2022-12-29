package aoc2022

import scala.io.Source


object Day11 extends App {
  val monkeyTemplate =
    """Monkey (\d):
      |  Starting items: (.*)
      |  Operation: new = (old [+*] .*)
      |  Test: divisible by (\d+)
      |    If true: throw to monkey (\d+)
      |    If false: throw to monkey (\d+)""".stripMargin.r

  class Monkey(val index: Int, val op: (Long => Long), val divider: Int
               , val test: (Long) => Int, var items: Vector[Long], var count: Long = 0) {
    def doRound(monkies: List[Monkey], worryFun: Long => Long): Unit = {
      //println(s"start $index ${items.mkString(", ")}")
      items.foreach(i => {val newValue = op.andThen(worryFun)(i)
        count = count +1
        val newIndex = test(newValue)
        //println(s"$index throwning $i to $newIndex")
        monkies(newIndex).transfer(newValue)})
        items = Vector.empty[Long]
    }

    def transfer(newItem: Long) = {
      items = items :+ newItem
      //println(s"$index received $newItem")
    }
  }

  object Monkey {

    def parse(lines: String): Monkey = {
      //println(s"lines [$lines]")
      val monkeyTemplate(index, items, operation, div, trueThrow, falseThrow) = lines
      new Monkey(index.toInt, parseFunction(operation), div.toInt, testFunction(div.toInt) ((b: Boolean) => if (b) trueThrow.toInt else falseThrow.toInt)
        , items.split(", ").map(_.toLong).toVector)
    }

    def testFunction(div: Int)(pred: (Boolean) => Int)(x: Long): Int = {
      pred(x % div == 0)
    }

    def parseFunction(func: String): (Long) => Long = {
      val parsed = func.split(" ").toList
      parsed match {
        case List("old", "+", addition) => (x: Long) => x + addition.toIntOption.fold(x)(_.toLong)
        case List("old", "*", other) => (x: Long) => x * other.toIntOption.fold(x)(_.toLong)
      }
    }
  }

  val test = Source.fromInputStream(this.getClass.getResourceAsStream("day11.txt")).getLines().toList

  val real = aocutils.getFromAOC(2022,11).split("\n").toList
  val printRounds = List(1,20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
  def part(lines: List[String], count: Int): Unit = {
    val monkies = (lines.mkString("\n").split("\n\n").map(ls => Monkey.parse(ls))).toList
    val worryFun = if (count > 20) { // part2
      val divProduct = monkies.map(_.divider).product
      (level: Long) => level % divProduct
    } else {
      (level: Long) => level / 3
    }
    (1 to count).foreach(c => {monkies.foreach(m => {m.doRound(monkies, worryFun);});
    if (printRounds.contains(c)) println(s"inspected $c (${monkies.map(_.count).mkString(", ")})")})
    println(s": ${monkies.map(m => m.count).sorted.reverse.take(2).product}")
  }

  part(test, 20)//, (i: Long) => i/3)
  part(real, 20) //, (i: Long) => i/3)
  part(test, 10000)
  part(real, 10000)
}
