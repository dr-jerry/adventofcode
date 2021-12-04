package aoc2021

import scala.io.Source

object Day03 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day03.txt")).getLines().toList

  def toNr(array: Array[Int], predicate: (Int) => String): Int = {
    Integer.parseInt(array.toList.map(t => predicate(t)).mkString(""), 2)
  }

  var countArray = Array.fill[Int](12)(0)
  array.foreach(f => f.split(""))
  array.foreach(bin => {
    val nr = bin.split("")
    for(i <- 0 to nr.length-1) {
      if(nr(i) == "1") {
        countArray(i) = countArray(i) + 1
      }
    }
  })
  println(toNr(countArray, (p: Int) => { if (p > array.size/2) "1" else "0" })
   * toNr(countArray, (p: Int) => { if (p < array.size/2) "1" else "0" }))


  def filterP(c: Char, index: Int) (value: String): Boolean = {
    value.charAt(index) == c
  }

  def filter(array: List[String], index: Int, c: Char): Int = {
    if (array.size == 1 || index > 11) Integer.parseInt(array(0), 2)
    else {
      val (l1, l2) = array.partition(filterP(c, index))
      if (c == '1')
        if (l1.size >= l2.size) filter(l1, index + 1, c)
        else filter(l2, index + 1, c)
      else if (l1.size <= l2.size) filter(l1, index + 1, c)
      else filter(l2, index + 1, c)
    }
  }

  println(s" step2 = ${filter(array, 0, '1') * filter(array, 0, '0')}")

}
