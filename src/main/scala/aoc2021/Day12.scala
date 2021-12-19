package aoc2021

import aocutils._

import scala.io.Source

object Day12 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream( "day12.txt")).getLines()
  def run(array: List[String]): (String, String) = {

    //var caves = Map[String, Set[String]]()
    // build a double Map a->b a-> +b , b-> +a
    val pattern = "(.*)\\-(.*)".r
    def parsePairs(list: List[String], map: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (list.isEmpty) map
      else {
        val pattern(a, b) = list.head
         parsePairs(list.tail, map + (a -> (map.getOrElse(a, Set()) + b), b -> (map.getOrElse(b, Set()) + a)))
      }
    }

    val caves: Map[String, Set[String]] = parsePairs(array, Map[String, Set[String]]())
    println(s"caves $caves")

    def findEnd(currentNode: String, nodes: Set[String]
                , road: List[String], roads: List[String], smallCavesVisited: Int): List[String] = {
      if (nodes.isEmpty) roads
      else if (nodes.head == "end") {
        findEnd(currentNode, nodes.tail, road
          , s"${(("end", "-") :: (currentNode, "end") :: road).reverse.mkString(",")}" :: roads, smallCavesVisited)
      } else {
 //       val segment = (currentNode,nodes.head)
        val left = smallCavesVisited - (if (nodes.head == "start") (1+smallCavesVisited)
          else
            if (nodes.head.toLowerCase() == nodes.head) road.count(nd => nd == nodes.head)
            else 0)
        if (left < 0)
          findEnd(currentNode, nodes.tail, road, roads, smallCavesVisited)
        else {
            findEnd(currentNode, nodes.tail, road,
              findEnd(nodes.head, caves(nodes.head), currentNode :: road
                , roads, left), smallCavesVisited)
        }
      }
    }
    val wayz = findEnd("start", caves("start"), List[String](), List[String](), 0)
    //println(wayz.mkString("\n"))
    ("part1: " + wayz.size.toString
      , "part2: " + findEnd("start", caves("start"), List[String](), List[String](), 1).size.toString)
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
  println(run(test1.toList))
  println(run(test2.toList))
  println("t3: " + run(test3.toList))
  println(run(array.toList))
}
