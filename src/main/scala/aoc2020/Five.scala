package aoc2020

import scala.io.Source

object Five extends App {
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("five.txt")).getLines.toList

  val plane = collection.mutable.Map((0 to 127).map(_ -> 0): _*)
  //val lines = List("BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL")
  val ids = lines.map(str => {
    val r = Integer.parseInt(str.substring(0, 7)
      .replace('F', '0').replace('B', '1'), 2)
    val c = Integer.parseInt(str.substring(7)
      .replace('R', '1').replace('L', '0'), 2)
    plane(r) = plane(r) + c
    //(c, r, 8*r+c)}))
    8 * r + c
  })
  println(ids)
  //plane.keys.toList.filter(i => !plane(i).equals(32)).foreach(println(pla)


}
