package aoc2020

import scala.io.Source

object Three extends App {
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/three.txt")).getLines().toList

  def traverse(dx: Int, dy: Int): Int = {
    var x = 0;
    var y = dy;
    var result = 0
    while (y < lines.size) {
      x += dx
      val line = lines(y)
      y += dy
      if (line.charAt(x % line.length) == '#') result += 1
    }
    result
  }

  //    while (lines.hasNext){
  //    val line = { lines.next();lines.next() }
  //    val chars = line.toCharArray
  //    val index = x % line.length
  //    x += 1
  //    if (chars(index) == '.')
  //      chars(index) = 'O'
  //    else {
  //      chars(index) = 'X'
  //      count += 1
  //    }
  //    println(s"$x ${x / line.length} ($count): ${line * (x / line.length)}${chars.mkString}")
  //  }
  // 3,1 259
  // 1,1 64
  // 5,1 65
  // 7,1 59
  // 1,2 30
  val pairs = List((3, 1), (1, 1), (5, 1), (7, 1), (1, 2))
  println(pairs.map(p => traverse(p._1, p._2)).map(BigInt(_)).product)
  //  println(traverse(3,1))
}
