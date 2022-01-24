package aoc2021

import aocutils._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day10 extends App {


  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day10.txt")).getLines()

    val c2o = Map[Char, Char](')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
    val o2c = c2o.map(_.swap)
    val scores = Map[Char, Long](')'-> 3L, ']'-> 57L, '}'-> 1197L, '>' -> 25137L)
    val p2scores = Map[Char, Long]('('-> 1L, '['-> 2L, '{'-> 3L, '<' -> 4L)
    val opens = c2o.values.toSet
    val closes = c2o.keys.toSet


    val dist: mutable.Map[String, List[String]] = mutable.Map[String, List[String]]()
    def parse(str: String, stack: List[Char]): (Boolean, Option[String], List[Char]) = {
      println(s"string is $str")
      if (str.isEmpty) (true, None, stack)
      else {
        if (closes.contains(str(0))) {
         // println(s"${str(0)} & $stack")
          if (stack(0) != c2o(str(0))) (false, Some(str), stack)
          else parse(str.tail, stack.tail)
        } else parse(str.tail, str(0) :: stack)
      }
    }

    val parts = array.map(line => parse(line, List())).partition(tp => {
      tp._1}) match { case (valid, error) => { (error
      .map(tup => tup._2.fold(0L)(str => scores.getOrElse(str(0), 0L))).sum
      , valid.map(tup => {tup._3.foldLeft(0L)((result, char)=> 5* result.toLong + p2scores(char))}).toList)}}
   println(s"part1: ${parts._1} \n ${parts._2.sortWith(_>_)(parts._2.size/2)}")
}
