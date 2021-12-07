package aoc2021

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day04 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day04.txt")).getLines().toList
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
    val all = (transposed ++ rows).map(a => ArrayBuffer(a:_*))
    var won = false

    def remove(element: Int, bnr: Int) = {
      if (!won) {
        for (i <- 0 to all.size-1) {
          all(i) -= element
          if (all(i).size == 0) {
            println(s"found! $bnr, $i, $element-> ${all.map(_.sum).sum /2 * element}")
            won = true
          }
        }
      }
    }
  }


  val blocks = iterate(array.drop(2), new ListBuffer[String](), List[Block]())
  array(0).split(",").map(_.toInt).foreach(nr => blocks.zipWithIndex.foreach( { case(b,i) => b.remove(nr, i) }))
}
