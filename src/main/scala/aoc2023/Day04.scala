package aoc2023

import scala.io.Source

object Day04 extends App {

  def part1(list: List[String]): Long = {
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

    var result = 0L
    val parsed = list.map(s => {
      val s"Card $id: $contents | $winning" = s.trim
      val nr = contents.trim.split(" ").intersect(winning.trim.split(" +")).size
      if (nr == 0) 0
      else Math.pow(2, nr - 1).toLong
    })
    println(parsed)
    parsed.sum
  }


  def part2(int: Int, mp: Map[Int,(Int, Long)]): Long = {
    //(1 to mp.size).foreach(i => println(s"$i -> ${mp.get(i).get}"))
    if (int == mp.size) mp.values.map(t => t._2).sum
    else {
      val (count, total) = mp.get(int).get

      val newMp = (int to int + count-1).map(t => (t+1, (mp.get(t+1).get._1, mp.get(t+1).get._2 + total))).toMap
      //println(s"$int -> $count & $total;${newMp.toList.mkString(", ")}")
      part2(int + 1, mp ++ newMp)
    }
  }

  //val real = Source.fromInputStream(this.getClass.getResourceAsStream("day04.txt")).getLines().toList
  val real = aocutils.getFromAOC(2023, 4).split("\n").toList
  println("part1: " + part1(real))
  val initMap = real.map(s => {
    val s"Card $id: $contents | $winning" = s.trim
    val nr = contents.trim.split(" ").intersect(winning.trim.split(" +")).size
    (id.trim.toInt, (nr -> 1L))
  }).toMap
  println(part2(1, initMap))


  //println("part2: " + part2(real))
  //
  //println(s"outcome = ${real.map(l => toNum(l)).sum}")
  //println(s"2 outcome = ${part2(real)}")

}
