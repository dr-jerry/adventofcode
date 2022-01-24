package aoc2021

import scala.io.Source

trait Packet2 {
  def version: Int
}
case class Literal2(value: String, version: Int) extends Packet2
case class Operator2(otype: String, version: Int, args: List[Packet2], bitSize: Int = 0, nrOfArgs: Int = 0) extends Packet2

object Day16partA extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day16.txt")).getLines().toList
  def paddedBin(hex: String): String = {
    String.format("%4s", Integer.toBinaryString(Integer.valueOf(hex.toString, 16))).replace(' ', '0')
  }

  def takeLiteral2(binary: String, value: String): (String, String) = {
    println(s"value is $value  binary: ${binary.substring(1,5)} int: ${Integer.valueOf(binary.substring(1,5), 2)}" )
    if (binary.startsWith("0")) (binary.substring(5), value + Integer.valueOf(binary.substring(1,5), 2))
    else takeLiteral2(binary.substring(5), value + Integer.valueOf(binary.substring(1,5),2))
  }

  def parseBinary(binary: String, accum: List[Packet2]): List[Packet2] = {
    println(s"binary is $binary ${if (binary.size > 19) Integer.toHexString(Integer.valueOf(binary.substring(0, 20),2))}")
    if (binary.size < 11) {println(s"something is wrong with $binary"); accum}
    else {
      val (version, rem1) = binary.splitAt(3)
      val (opval, rem2) = rem1.splitAt(3)
      println(s"version is $version, operator $opval")
      if (opval == "100") {
        val (binaryRemainder, litval) = takeLiteral2(rem2, "")
        parseBinary(binaryRemainder, Literal2(litval, Integer.valueOf(version, 2)) :: accum)
      } else {
        val next = if (rem2.startsWith("1")) 11 + 1 else 15 + 1;
        val operaator = if (next ==16) {
//          println(s"looking at 15 (bitsize) ${Integer.valueOf(rem2.substring(1,next),2)}")
          val size = Integer.valueOf(Integer.valueOf(rem2.substring(1,next),2))
          println(s"going to args with $size and ${rem2.substring(next,size)}")
          val argList = parseBinary(rem2.substring(next, size+next),List[Packet2]())
          Operator2(Integer.valueOf(opval, 2).toString, Integer.valueOf(version, 2)
            , argList, bitSize = Integer.valueOf(rem2.substring(1,next),2))
        } else {
//          println(s"looking at 11 nrofargs ${Integer.valueOf(rem2.substring(1,next),2)}")
          Operator2(Integer.valueOf(opval, 2).toString, Integer.valueOf(version, 2)
            , List[Packet2](), nrOfArgs = Integer.valueOf(rem2.substring(1,next),2))
        }
        println(s"operator: $operaator")
        val newList = operaator :: accum
        parseBinary(rem2.substring(next), newList.toList)
      }
    }
  }
  val l = parseBinary(array(0).toList.map(hex => paddedBin(hex.toString)).mkString, List[Packet2]())
  println(s"versions: ${l.map(t => t.version).mkString(", ")}")
  l.foldLeft(0)((ac, p) => p.version + ac)

  def run(array: List[Char]): (String, String) = {
    val lists = parseBinary(array.map(hex => paddedBin(hex.toString)).mkString, List[Packet2]())
    println(s"operators: $lists.reverse")
    (s"part1: ${lists.foldLeft(0)((ac, p) => p.version + ac)}", s"part2: ")
  }
  println(run(array(0).toList));
}
