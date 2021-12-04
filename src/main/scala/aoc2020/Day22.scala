package aoc2020

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.Queue
import scala.util.matching.Regex

trait Day22 {

  val Num: Regex = """(\d+)""".r

  def score(x: (Int, Int)): Int = x._1 * (x._2 + 1)

  def parse(input: Iterable[String]): (Queue[Int], Queue[Int]) = {
    val players = input.foldLeft(Nil: List[Queue[Int]]) {
      case (player, line) if line.startsWith("Player") => Queue[Int]() :: player
      case (player, Num(num)) => player.head.enqueue(num.toInt) :: player.tail
      case (player, _) => player
    }
    (players.tail.head, players.head)
  }

  def part1(input: Iterable[String]): Int = {
    @tailrec
    def recurse(p1: Queue[Int], p2: Queue[Int]): Queue[Int] = {
      if (p1.isEmpty) p2
      else if (p2.isEmpty) p1
      else {
        val ((play1, nextP1), (play2, nextP2)) = (p1.dequeue, p2.dequeue)
        if (play1 > play2) recurse(nextP1.enqueue(immutable.Iterable(play1, play2)), nextP2)
        else recurse(nextP1, nextP2.enqueue(immutable.Iterable(play2, play1)))
      }
    }

    val (player1, player2) = parse(input)
    recurse(player1, player2).reverse.zipWithIndex.map(score).sum
  }

  def part2(input: Iterable[String]): Int = {
    def recursiveCombat(p1: Queue[Int], p2: Queue[Int], seen: Set[(Int, Int)] = Set()): (Int, Seq[Int]) = {
      if (p2.isEmpty) (1, p1)
      else if (p1.isEmpty) (2, p2)
      else {
        val hash = (p1.hashCode(), p2.hashCode())
        if (seen.contains(hash)) (1, p1)
        else {
          val ((card1, p1next), (card2, p2next)) = (p1.dequeue, p2.dequeue)
          val winner =
            if (p1next.size >= card1 && p2next.size >= card2) recursiveCombat(p1next.take(card1), p2next.take(card2))._1
            else if (card1 > card2) 1 else 2
          if (winner == 1) recursiveCombat(p1next.enqueue[Int](immutable.Iterable(card1, card2)), p2next, seen + hash)
          else recursiveCombat(p1next, p2next.enqueue[Int](immutable.Iterable(card2, card1)), seen + hash)
        }
      }
    }

    val (player1, player2) = parse(input)
    recursiveCombat(player1, player2)._2.reverse.zipWithIndex.map(score).sum
  }
}
