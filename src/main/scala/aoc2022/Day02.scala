package aoc2022

import scala.io.Source

object Day02 extends App {
  // A,X(1) -> Rock, B,Y(2) -> Paper; C,Z(3) -> scissors 0 (lose), 3 (draw) , 6(win)
  var c2p1 = Map("A X" -> 4
    , "B X" -> 1
    , "C X" -> 7
    , "A Y" -> 8
    , "B Y" -> 5, "C Y" -> 2, "A Z" -> 3, "B Z" -> 9, "C Z" -> 6 )

  var array: List[String] = Source.fromInputStream(this.getClass.getResourceAsStream("day02.txt")).getLines().toList
  println("voor arno: " + array.map(comb => c2p1(comb)).sum)
//  println(s"part1 ${array.foldLeft(0)((score, comb) => {
//    score + c2p1(comb)
//  })}")

  //part2 X-Lose, Y-Draw, Z-Win
  var c2p2 = Map("A X" -> 3, "B X" -> 1, "C X" -> 2, "A Y" -> 4, "B Y" -> 5, "C Y" -> 6, "A Z" -> 8, "B Z" -> 9, "C Z" -> 7)
  println(s"part2 ${array.foldLeft(0)((score, comb) => {
    score + c2p2(comb)
  })}")

}
