package aoc2021

import scala.io.Source

trait Packet {
  def version: Int
}
case class Literal(value: String, version: Int) extends Packet
case class Operator(otype: String, version: Int, args: List[Packet]) extends Packet

object Day16 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day16.txt")).getLines().toList
  def paddedBin(hex: String): String = {
    String.format("%4s", Integer.toBinaryString(Integer.valueOf(hex.toString, 16))).replace(' ', '0')
  }

  def takeLiteral(binary: String, value: String): (String, String) = {
    println(s"value is $value  binary: ${binary.substring(1,5)} int: ${Integer.valueOf(binary.substring(1,5), 2)}" )
    if (binary.startsWith("0")) (binary.substring(5), value + Integer.valueOf(binary.substring(1,5), 2))
    else takeLiteral(binary.substring(5), value + Integer.valueOf(binary.substring(1,5),2))
  }

  def parseBinary(binary: String, accum: List[Packet]): List[Packet] = {
    println(s"binary is $binary ${if (binary.size > 19) Integer.toHexString(Integer.valueOf(binary.substring(0, 20),2))}")
    if (binary.size < 11) {println(s"something is wrong with $binary ${accum.mkString}"); accum}
    else {
      val (version, rem1) = binary.splitAt(3)
      println(s"version is $version")
      val (operator, rem2) = rem1.splitAt(3)
      if (operator == "100") {
        val (binaryRemainder, literalval) = takeLiteral(rem2, "")
        parseBinary(binaryRemainder, Literal(literalval, Integer.valueOf(version, 2)) :: accum)
      } else {
        val next = if (rem2.startsWith("1")) 11 + 1 else 15 + 1;
        val newList = Operator(Integer.valueOf(operator, 2).toString, Integer.valueOf(version, 2), List[Packet]()) :: accum
        parseBinary(rem2.substring(next), newList.toList)
      }
    }
  }
  val l = Day16.parseBinary(array(0).toList.map(hex => Day16.paddedBin(hex.toString)).mkString, List[Packet]())
  println(s"versions: ${l.map(t => t.version).mkString(", ")}")
  l.foldLeft(0)((ac, p) => p.version + ac)

  def run(array: List[Char]): (String, String) = {
    val lists = parseBinary(array.map(hex => paddedBin(hex.toString)).mkString, List[Packet]())
    (s"part1: ${lists.foldLeft(0)((ac, p) => p.version + ac)}", s"part2: ")
  }
  println(s"${run(array(0).toList)}")
}
