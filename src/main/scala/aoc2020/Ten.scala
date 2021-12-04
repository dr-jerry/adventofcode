package aoc2020

import scala.collection.mutable
import scala.io.Source

object Ten extends App {
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/ten.txt"))
    .getLines().map(_.toInt).toArray.sortWith(_ < _)
  println(lines.mkString(","))
  val results = mutable.Map[Int, Int](1 -> 0, 2 -> 0, 3 -> 0)
  //(1 to lines.length-1).foreach(i => {results(lines(i)-lines(i-1)) += 1})

  println(s"${results(1)} * ${results(3)}")
  println(71 * 31)

  // 10B
  def allowed(value: Int, index: Int, lastBit: Int): Boolean = {
    val str = s"${Integer.toString(value, 2)}\n${" " * (index - 1)}^  $index , $lastBit"
    println(str)
    if (index == 1)
      true
    else if ((lastBit - index) >= 3)
      false
    else if ((value >> index & 1) == 1)
      allowed(value, index - 1, index)
    else
      allowed(value, index - 1, lastBit)
  }

  def count2(count: Int): Int = {
    println("count2" + count)
    val totalPs = Math.pow(2, count).toInt
    (0 to totalPs / 2 - 1 by 2).filter(i => { //println(Integer.toString(totalPs / 2 +i +1,2))
      Integer.toString(totalPs / 2 + i + 1, 2).indexOf("000") == -1
    }).size
    //      println("here")
    //      val j = totalPs / 2 + i * 2+1;
    //      val r = allowed(j,count-1,count)
    //      println(s"$j - ${Integer.toString(j,2)} -> $r")
    //      r
    //    }).size
  }


  //  def count(pw: Int): Int = {
  //    val t = Math.pow(2, pw)/2
  //    (0 to (t/2).toInt).filter(i => { val j = i*2 + t +1;
  //      val ns = Integer.toString(j.toInt,2);
  //      println(s"$i -> $j -> s: $ns -> allowed: ${allowed(ns, 0)}")
  //    allowed(ns, 0)}).size
  //  }
  (33 to 63).foreach(j => {
    (0 to 5).foreach(i => println(s"${Integer.toString(j, 2)} $j $i ${(j.toInt >> i.toInt) & 1.toInt}\n${" " * (i)}^"))
  })

  println(Integer.toString(35, 2))
  //  allowed(35, 5, 6)
  println("hi " + count2(5))


  //var chunkSize = 1;
  var prev = -1;
  var chunk: Vector[Int] = Vector()
  var result: BigInt = 1;
  println(lines.mkString(","))
  (0 +: lines :+ 3 + lines(lines.length - 1)).foreach(i => {
    println(s"i is $i")
    if ((i - prev) == 1) {
      chunk = chunk :+ i
      prev = i;
    } else if ((i - prev) == 3) {
      if (chunk.size > 2) result *= count2(chunk.size)
      chunk = Vector(i)
    }
    prev = i;
  })
  println(s"result is $result")

}
