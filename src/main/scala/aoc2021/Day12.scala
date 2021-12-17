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

    def findEnd(currentNode: String, nodes: Set[String], road: List[(String, String)], roads: List[String]): List[String] = {
      println(s"current $currentNode + $nodes and road ${road.map(t => s"${t._1}->${t._2}").mkString("-")}")
      if (nodes.isEmpty) roads
      else if (nodes.head == "end") findEnd(nodes.head, nodes.tail, road
        , road.appended((currentNode, nodes.head)).mkString("-") :: roads)
      else {
        val segment = (currentNode,nodes.head)
        println()
        if (nodes.head.toLowerCase() == nodes.head && road.exists(t => t._1 == nodes.head)
          || road.exists(t => t == segment))
          findEnd(nodes.head, nodes.tail, road, roads)
        else findEnd(nodes.head, caves(nodes.head), road.appended(segment), roads) :::
          findEnd(currentNode, nodes.tail, road.appended(segment), roads)
      }
    }
//      else if (currentNode == "end") {
//        println(s"found!! $road + $roads")
//        findEnd(currentNode, nodes.tail, road.tail, roads :+ road.mkString(","))
//      } else {
//        val segment = s"'$currentNode'-'${nodes.head}'"
////        println(s"1      (${currentNode == "start" && !road.isEmpty}) 2:${nodes.isEmpty}\n"
////         + s" 3 ${currentNode.toLowerCase == currentNode && road.mkString("").contains(currentNode)}"
////         + s" 4 ${road.mkString("").contains(segment)}\n"
////         + s"5 ${road.contains(segment)}")
//
//        if ((currentNode == "start" && !road.isEmpty) || nodes.isEmpty
//          || currentNode.toLowerCase == currentNode && road.mkString("").contains(currentNode)
//          || road.mkString("").contains(segment)
//          || road.contains(segment)) {
//          //println("not joinging")
//          findEnd(nodes.head, nodes.tail, road, roads)
//
//        } else {
//          println("joining")
//          findEnd(nodes.head, caves(nodes.head), road.appended(segment), roads) :::
//            findEnd(currentNode, nodes.tail, road.appended(segment), roads)
//        }
//      }
//    }

    (findEnd("start", caves("start"), List[(String,String)](), List[String]()).size.toString, "two")
  }

val test1 = """start-A
    |start-b
    |A-c
    |A-b
    |b-d
    |A-end
    |b-end
    |""".stripMargin.split("\n")

  println(run(test1.toList))
  //println(run(array.toList))
}
