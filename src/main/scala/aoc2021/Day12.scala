package aoc2021

import aocutils._

import scala.io.Source

object Day12 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream( "day12.txt")).getLines()
  def run(array: List[String]): (String, String) = {
    val pattern = "(.*)\\-(.*)".r

    //var caves = Map[String, Set[String]]()
    // build a Map String -> Set
    def parsePairs(list: List[String], map: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (list.isEmpty) map
      else {
        val pattern(a, b) = list.head
        //          val s: Set[String] =
        //          val m: Map[String, Set[String]] =

        parsePairs(list.tail, map + (a -> (map.getOrElse(a, Set()) + b), b -> (map.getOrElse(b, Set()) + a)))
      }
    }

    val caves: Map[String, Set[String]] = parsePairs(array, Map[String, Set[String]]())
    println(s"caves $caves")
    val tp = "(.*) & (\\d+)".r

    def findEnd(currentNode: String, nodes: Set[String], road: List[(String, String)], roads: List[String], trace: String): List[String] = {
      val tp(tr,ind) = trace
      val index = ind.toInt
      //println(s"$tr,$index size ${roads.size}  current $currentNode + $nodes\n and roads ${roads.mkString("\n")}")
      if (nodes.isEmpty) {/* println(s"out $trace & $index and roads: ${roads.mkString("\n")}");*/ roads}
      else if (nodes.head == "end") {
        //println(s"END ${road.reverse.map(t => s"${t._1}->${t._2}").mkString("-")}");
        findEnd(currentNode, nodes.tail, road
          , s"${road.reverse.map(t => {t._1}).mkString(",")}" :: roads, s"after end & $index")
      } else {
        val segment = (currentNode,nodes.head)
        if (nodes.head.toLowerCase() == nodes.head && road.exists(t => t._1 == nodes.head)
          || road.exists(t => t == segment))
          findEnd(currentNode, nodes.tail, road, roads, s"exists & $index")
        else {
            findEnd(currentNode, nodes.tail, road,
              findEnd(nodes.head, caves(nodes.head), segment :: road
                , roads, s"cave in & ${index+1}"), s" tail & ${index}")
        }
      }
    }
    val wayz = findEnd("start", caves("start"), List[(String,String)](), List[String](), "start & 0")
    //println(wayz.mkString("\n"))
    (wayz.size.toString, "part2")
  }

val test1 = """start-A
    |start-b
    |A-c
    |A-b
    |b-d
    |A-end
    |b-end
    |""".stripMargin.split("\n")
val test2 = """dc-end
    |HN-start
    |start-kj
    |dc-start
    |dc-HN
    |LN-dc
    |HN-end
    |kj-sa
    |kj-HN
    |kj-dc
    |""".stripMargin.split("\n")

  val test3="""fs-end
              |he-DX
              |fs-he
              |start-DX
              |pj-DX
              |end-zg
              |zg-sl
              |zg-pj
              |pj-he
              |RW-he
              |fs-DX
              |pj-RW
              |zg-RW
              |start-pj
              |he-WI
              |zg-he
              |pj-fs
              |start-RW""".stripMargin.split("\n")
  //println(run(test1.toList))
  //println(run(test2.toList))
  //println("t3: " + run(test3.toList))
  println(run(array.toList))
}
