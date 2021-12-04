package aoc2020

import scala.io.Source

object Eleven extends App {

  case class Coor(val x: Int, val y: Int)

  var array = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/eleven.txt")).getLines()
    .map(_.toCharArray).toArray

  val boundary = Coor(array(0).size - 1, array.size - 1)

  val delta = List(Coor(-1, -1), Coor(0, -1), Coor(1, -1),
    Coor(-1, 0), Coor(1, 0),
    Coor(-1, 1), Coor(0, 1), Coor(1, 1))

  def evaluate(field: String): String = {
    println(s"evaluating\n$field")
    array = field.split("\n").map(s => s.toCharArray)
    val newField = (0 to boundary.y).map(y => {
      (0 to boundary.x).map(x =>
        (array(y)(x), countNeighbours(array, x, y)) match {
          case ('.', c) => "."
          case ('#', c) if (c >= 5) => "L"
          case ('L', 0) => "#"
          case (c, _) => c.toString
        }).mkString("")
    }).mkString("\n")
    if (!newField.equals(field))
      evaluate(newField)
    else
      field
  }

  def countNeighbours(array: Array[Array[Char]], x: Int, y: Int): Int = {
    var result: Int = 0
    //    println(s"$x, $y -> ${array(y)(x)}")
    var i = 1;
    if (array(y)(x) != '.') {
      var reslt = 0;
      delta.foreach(d => result += {
        try {
          i = 1
          reslt = 0;
          while (i != 0) {
            val c = array(y + i * d.y)(x + i * d.x)
            if (c == '#') {
              reslt = 1;
              i = 0;
            } else if (c == 'L') {
              reslt = 0
              i = 0;
            } else if (c == '.') {
              i += 1
            }
          }
        } catch {
          case _ => {
            i = 0
          }
        }
        reslt
      })
    } else
      result = -1
    result
  }

  val end = evaluate(array.map(ac => ac.mkString("")).mkString("\n"))

  println(s"endstring is \n$end, ${end.count(_ == '#')}")
}
