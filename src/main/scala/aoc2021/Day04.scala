package aoc2021

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day04 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day04-tst.txt")).getLines().toList
  val input = array(0).split(",").map(_.toInt)

  def iterate(lines: List[String], blockLines: ListBuffer[String], blockList: List[Block]): List[Block] = {
    lines match {
      case head :: tail => {
        if (blockLines.size == 5) {
          iterate(tail, ListBuffer[String](), blockList.appended(new Block(blockLines)))
        } else {
          iterate(tail, blockLines :+ head, blockList)
        }
      }
      case _ => blockList :+ new Block(blockLines)
    }
  }
  class Block(lines: ListBuffer[String]) {
    var rows = lines.map(str => str.trim.split(" +").map(_.toInt)).toArray
    //lines.foreach(s => println(s))
    var transposed = rows.transpose
    val all = (transposed ++ rows).map(a => ArrayBuffer(a: _*))
    println(s"transposed ${all.foreach(s => println(s.mkString(",")))}")

    def remove(element: Int) = {
      for (i <- 0 to all.size-1) {
        all(i) -= element
        if (all(i).size == 0) {
          println(s"found! $i -> ${all.map(_.sum).sum /2 * element}")
        }
      }
    }
  }


  val blocks = iterate(array.drop(2), new ListBuffer[String](), List[Block]())
  array(0).split(",").map(_.toInt).foreach(nr => blocks.foreach(b => b.remove(nr)))
}
