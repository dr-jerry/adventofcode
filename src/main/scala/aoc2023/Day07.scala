package aoc2023

import scala.io.Source

case class Card (rank: List[(Int, Char)], stringRank: String, hand: String, bid: Long )
case class Card2 (rank: List[(Char, Int)], typeRank: String, cardRank: String, hand: String, bid: Long, jokers: Int )
object Day07 extends App {
  implicit object CardCompareOrder extends math.Ordering[Card] {
    def compare(a: Card, b: Card) = {
      a.stringRank.compareTo(b.stringRank)
    }

  }
//  val real = Source.fromInputStream(this.getClass.getResourceAsStream("day07.txt")).getLines().toList
  val real = aocutils.getFromAOC(2023, 7).split("\n").toList
  def part1(list: List[String]): Long = {
    val labels = "23456789TJQKA"
    val g = list.map(s => s.split(" +")).map(ar => { val rank = ar(0).toCharArray.groupBy(identity).view.mapValues(_.size)
      .toList.map(t => (t._2, t._1)).sortWith((t1, t2) => if (t1._1 == t2._1) labels.indexOf(t1._2) > labels.indexOf(t2._2) else t1._1 > t2._1)
      val stringRank = rank.map(rc => rc._1).mkString("") + " " + ar(0).map(c => Integer.toString(labels.indexOf(c),16)).mkString("") +
         " " + rank.map(rc => Integer.toString(labels.indexOf(rc._2), 16)).mkString("")
      Card(rank, stringRank, ar(0), ar(1).toLong)})
    g.sortWith((c1, c2) => c1.stringRank < c2.stringRank).zipWithIndex.map{case (card, i) => card.bid * (i+1)}.sum
  }

  def part2(list: List[String]): Long = {
    val labels = "J23456789TQKA"
    val g = list.map(s => s.split(" +")).map(ar => { val rank = ar(0).toCharArray.filter(c => c != 'J').groupBy(identity).view.mapValues(_.size)
      .toList.sortWith((t1, t2) => if (t1._2 == t2._2) labels.indexOf(t1._1) > labels.indexOf(t2._1) else t1._2 > t2._2)
      // jokers are filtered out, nr of jokers is determined.
      val jokers = 5 - rank.map{ case (c, i) => i}.sum
      val jokerList = try {
        (rank.head._1, rank.head._2 + jokers) :: rank.tail
      } catch {
        case _ => println(s"error with ${ar(0)}");List(('J',5))
      }
      Card2(rank, jokerList.map(rc => rc._2).mkString(""), ar(0).map(c => Integer.toString(labels.indexOf(c),16)).mkString(""), ar(0), ar(1).toLong, jokers)})
    val newG = g.sortWith((c1, c2) => {
      if (c1.typeRank == c2.typeRank)
       c1.cardRank < c2.cardRank
      else c1.typeRank < c2.typeRank
    })
    newG.foreach(ng => println(ng) )
    newG.zipWithIndex.map{case (card, i) => card.bid * (i+1)}.sum
  }

  println(s"part1: ${part1(real)}")
  println(s"part2: ${part2(real)}")

 }
