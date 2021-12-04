package aoc2020

import scala.collection.mutable.ListBuffer
import scala.io.Source

object TwentyTwo extends App {
  val pLength = 25
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/twentytwo.txt")).getLines().toList

  val players = List(new ListBuffer[Int](), new ListBuffer[Int])
  var focus = 0
  val PlayerRE = "Player (\\d):".r
  lines.foreach(line => {
    line match {
      case PlayerRE(i) => focus = i.toInt - 1
      case _ if line.isEmpty => ()
      case l: String => players(focus).append(l.toInt)
    }
  })

  def calculate(lb: ListBuffer[Int]): BigInt = {
    lb.zipWithIndex.map { case (c, i) => {
      (lb.size - i) * c
    }
    }.sum
  }

  case class Result(winner: Int, solution: BigInt)

  def play(decks: List[ListBuffer[Int]], visited: List[Set[String]]): Result = {
    val newVisited = List(visited(0) + decks(0).mkString(""), visited(1) + decks(1).mkString(""))
    //println(decks)
    if ((0 to 1).exists(i => visited(i).contains(decks(i).mkString(""))))
      Result(0, calculate(decks(0)))
    else if (decks(0).isEmpty)
      Result(1, calculate(decks(1)))
    else if (decks(1).isEmpty)
      Result(0, calculate(decks(0)))
    else {
      val result = if (decks(0).head <= (decks(0).size - 1) && decks(1).head <= (decks(1).size - 1)) {
        println(s"in sub game ${decks(0).size} and ${decks(1).size}")
        play(List(decks(0).clone().subtractOne(decks(0).head)
          , decks(1).clone().subtractOne(decks(1).head)), List(Set[String](), Set[String]()))
      } else if (decks(1).head > decks(0).head) {
        Result(1, 0)
      } else Result(0, 1)
      if (result.winner == 1) {
        val losingHead = decks(0).head
        play(List(decks(0).subtractOne(losingHead),
          decks(1).append(decks(1).head).append(losingHead)
            .subtractOne(decks(1).head)), newVisited)
      } else {
        play(List(decks(0).append(decks(0).head).append(decks(1).head)
          .subtractOne(decks(0).head)
          , decks(1).subtractOne(decks(1).head)), newVisited)
      }
    }
  }

  println(play(players, List(Set[String](), Set[String]())))
}
