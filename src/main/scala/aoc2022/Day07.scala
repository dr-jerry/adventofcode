package aoc2022

import scala.io.Source

object Day07 extends App {
  val maks = BigInt(70000000)
  val min = BigInt(30000000)

  case class Node(name: String, list: List[Node], size: BigInt)
  val cdre = "^\\$ cd (.+)".r
  val lsre = "^\\$ ls".r
  val sizere = "^(\\d+) (.+)".r

  def parseList(commands: List[String], acc: Node):(Node,List[String]) = {
    commands match {
      case kop :: staart => {
 //       println(s"acc : $acc")
 //       println(s"kop: '$kop' staart $staart")
        kop match {
          case cdre(name) if (name == "..") =>  (acc, staart)
          case cdre(name) if (staart.head != "$ ls") => {println("cd without ls");(Node("error", List.empty[Node], 0), List.empty[String])}
          case cdre(name) =>
            val newNode = parseList(staart, Node(name, List.empty[Node], 0))
            val updated = acc.copy(list = acc.list :+ newNode._1, size = acc.size + newNode._1.size)
            parseList(newNode._2, updated)
          case sizere(sz,name) => parseList(staart, acc.copy(size = acc.size + BigInt(sz.toInt)))
          case _ => parseList(staart,acc)
        }
      }
      case _ => (acc, List.empty[String])
  }}

  def sumUp(node: Node, acc: BigInt = 0) : BigInt ={
    val toAdd = if (node.size <= 100000) node.size else BigInt(0)
    //println(s"node: $node, toAdd $toAdd")
    toAdd + node.list.map (sumUp(_, acc) ).sum
  }
  def findLeast(node: Node, th: BigInt, current: BigInt) : BigInt = {
    //println(s"findLeast node( ${node.name} , ${node.list.size}, ${node.size}), threshold: $th, current $current")
    val result = node.list.foldLeft(current)((found, nd) => {
      if (nd.size > th && nd.size < found) findLeast(nd, th, nd.size)
      else findLeast(nd, th, found)
    })
    //println(s"result: $result")
    result
  }

  val test = Source.fromInputStream(this.getClass.getResourceAsStream( "day07.txt")).getLines().toList
  val real = aocutils.getFromAOC(2022,7).split("\n")
  //println(s"part1: ${stacks.toList.map(l => l.substring(0,1)).mkString}")
  def part1(lines: List[String]): Int = {
    val node = parseList(lines.tail, Node("/", List.empty[Node], 0))
    println(s"finished: " +node._1)
    println(sumUp(node._1))
    val req = min - (maks - node._1.size)
    println(s"unused: ${maks - node._1.size} required: ${min - (maks - node._1.size) }")
    println(s"part2: ${findLeast(node._1, req, node._1.size)}")
    0
  }


//    .map(ca => {println(s"ca is $ca");ca.filter(c => {println("c is " + c); c != ' '}).toString}).foreach(println(_))
  part1(test)
  println("for real:")
  part1(real.toList)
}
