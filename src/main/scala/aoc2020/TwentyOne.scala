package aoc2020

import scala.annotation.tailrec
import scala.collection.Map
import scala.io.Source

object TwentyOne extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/twentyone.txt")).getLines().toList

  val linePattern = "(.*) \\(contains (.*)\\)$".r

  var allIngredients = List[String]()

  @tailrec
  def process(lines: List[String], aller2Ing: Map[String, List[String]], all: List[String]): Map[String, List[String]] = {
    lines match {
      case line :: tail => {
        val linePattern(ingText, allergenes) = line
        val ingredients = ingText.split(" ")
        allIngredients ++= ingredients
        //print(allIngredients)
        val map = allergenes.split(", ").map(al => (al -> aller2Ing.get(al)
          .fold(ingredients.toList)(l => l.intersect(ingredients)))).toMap
        process(tail, aller2Ing ++ map, all ++ ingredients)
      }
      case line :: tail if line.isEmpty => aller2Ing
      case _ => aller2Ing + ("total" -> List[String](aller2Ing.values.flatMap(is =>
        is.map(i => {
          println(all); all.filter(_ != i)
        })).size.toString))
    }
  }

  val dangerous = process(array, Map(), List())
  for ((k, v) <- dangerous) println(s"$k -> $v")
  println(dangerous("total"))
  dangerous.values.foreach(is => is.foreach(i => {
    println(allIngredients); allIngredients = allIngredients.filter(_ != i)
  }))
  println(allIngredients.size)

}
