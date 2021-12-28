package aoc2021

import aocutils.Point

import scala.io.Source

object Day14 extends App {
  val pairPattern = "([A-Za-z])([A-Za-z]) -> ([A-Za-z])".r
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day14.txt")).getLines().toList

  def run(array: List[String]): (String, String) = {
    println("Arno's som is " + Integer.valueOf("10101111000010",2))
    val (template, pairs) = array.filter(_.size != 0).partition(!_.contains("->"))
    val pair2trip = pairs.map(str => {
      val pairPattern(left, right, insert) = str
      (s"$left$right" -> s"$insert$right")
    }).toMap
    def pair2Tup(code: String): (String, String) = {
      val p2T = pair2trip(code)
      (s"${code(0)}${p2T(0)}", p2T)
    }



    def toDub(template: List[Char], accum: Map[String,BigInt]): Map[String, BigInt] = {
      template match {
        case head :: tail if (tail.size > 0) => (toDub(tail, accum + (s"$head${tail(0)}" -> accum.getOrElse(s"$head${tail(0)}", BigInt(0)).+(BigInt(1)))))
        case _ => accum
      }
    }

    def proCount(dubs: List[(String, BigInt)], accum: Map[String, BigInt]): Map[String, BigInt] = {
      if (dubs.isEmpty) accum map {case (str, b) => (str, b.+(BigInt(1))./(BigInt(2)))}
      else {
        val (first, second) = dubs.head._1.splitAt(1)
        //println(s"procount $first, $second")
        val fMap = accum + (first -> accum.getOrElse(first,BigInt(0)).+(dubs.head._2))
        val sMap = fMap + (second -> fMap.getOrElse(second, BigInt(0)).+(dubs.head._2))
        //println(s"procount smap : $sMap")
        proCount(dubs.tail, sMap)
      }
    }

    def proScan(dubs: Map[String, BigInt]): Map[String, BigInt] = {
      //println(s"dubs: $dubs")
      def recur(keys: Iterable[String], accum: Map[String, BigInt]): Map[String, BigInt] = {
        if (keys.isEmpty) accum
        else {
          val newKeys = pair2Tup(keys.head)
          //println(s"head: ${keys.head}. new keys: ${newKeys}")
          val newMap = accum + (newKeys._1 -> accum.getOrElse(newKeys._1, BigInt(0)).+(dubs.getOrElse(keys.head, BigInt(0)))) //accum.getOrElse(newKeys._1, BigInt(0)) + BigInt(1)),
          val newMap2 = newMap + (newKeys._2 -> newMap.getOrElse(newKeys._2, BigInt(0)).+(dubs.getOrElse(keys.head, dubs.getOrElse(keys.head, BigInt(0)))))
          //println(s"new Map $newMap2")
          recur(keys.tail, newMap2)
        }
      }

      recur(dubs.keys, Map[String, BigInt]())
    }

    def scan(template: List[Char], accum: String): String = {
//      println(s"template $template" + accum)
//      println(s"template ${template.mkString("")}")
      template match {
        case head :: tail if (tail.size > 0) => (scan(tail, accum + pair2trip(s"$head${tail(0)}")))
        case _ => accum
        }
      }

    def iterate(count: Int, template: String):String = {
      var result = scan(template.toList, template(0).toString)
      println("result " + result.toList.groupBy(identity).view.mapValues(_.size).toList.sortWith(_._2>_._2))
      if (count > 0) iterate(count-1, result)
      else {
        result
      }
    }
    var tmp = toDub(template(0).toList, Map[String, BigInt]())
    println(s"in dub ${template(0)} -> $tmp")
    for (i <- 0 to 39) {
      tmp = proScan(tmp);
      val result = proCount(tmp.toList, Map[String, BigInt]())
      println(s"counts[$i] ${result.toList.sortWith(_._2>_._2)}")
      val sortedList = result.values.toList.sortWith(_>_)
      println(s"diff[$i] ${sortedList(0)} - ${sortedList(sortedList.size-1)} = ${(sortedList(0) -sortedList(sortedList.size-1))}")
    }

    val result = toDub(template(0).toList, Map[String, BigInt]())
    println("result " + result)
    (s"p1", s"p2: ${proScan(result)}")
  val resultMap = iterate(9, template(0)).toList.groupBy(identity).view.mapValues(_.size)
    println(resultMap.toMap)
    val sortedList = resultMap.toList.sortWith((a,b) => a._2>b._2)
    ("part1:" + (sortedList(0)._2 -sortedList(sortedList.size-1)._2) , "part2")
  }

val test1 = """NNCB
              |
              |CH -> B
              |HH -> N
              |CB -> H
              |NH -> C
              |HB -> C
              |HC -> B
              |HN -> C
              |NN -> C
              |BH -> H
              |NC -> B
              |NB -> B
              |BN -> B
              |BB -> N
              |BC -> B
              |CC -> N
              |CN -> C""".stripMargin.split("\n").toList
  //println(run(test1))
  println(run(array))
}
