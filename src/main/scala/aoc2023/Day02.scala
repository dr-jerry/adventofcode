package aoc2023
import scala.io.Source

object Day02 extends App {

  val bag = Map("red"  -> 12, "green" -> 13, "blue" -> 14)
  def part1(list: List[String]): Long = {
    var result = 0L
    val parsed = list.map(s => {
      val s"Game $id: $contents" = s
      (id.toInt,contents.split("[;,]").exists(line => {
        val s"$count $color" = line.trim
        bag.get(color).get < count.toInt}))})
    parsed.filter(t => !t._2).map(t => t._1).sum
  }

  def part2(list: List[String]): Long = {
    list.map(s => {
      val s"Game $id: $contents" = s
      contents.split("[;,]").map(str => {val s"$count $color" = str.trim;
        (color, count.toLong)}).foldLeft(Map("red"  -> 0L, "green" -> 0L, "blue" -> 0L)){(map, tup) =>
        if (map.get(tup._1).get < tup._2) map ++ Map(tup._1 -> tup._2) else map}})
      .map(mp => mp.values.product).sum
    }

  val real = Source.fromInputStream(this.getClass.getResourceAsStream("day02.txt")).getLines().toList
  //val real = aocutils.getFromAOC(2023, 2).split("\n").toList
  println("part1: " + part1(real))
  println("part2: " + part2(real))
  //
  //println(s"outcome = ${real.map(l => toNum(l)).sum}")
  //println(s"2 outcome = ${part2(real)}")

}
