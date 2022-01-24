package aoc2021

import scala.collection.mutable.ListBuffer
import scala.io.Source

trait Packet {
  def version: Int
}
case class Literal(value: String, version: Int) extends Packet
case class Operator(otype: String, version: Int, args: List[Packet]) extends Packet {
  override
  def toString: String = {
    val op = otype match {
      case "0" => '+'
      case "1" => '*'
      case "2" => '_'
      case "3" => 'x'
      case "5" => '>'
      case "6" => '<'
      case "7" => '='
    }
    s"Operator( $op/$otype $version, $args)"
   }
}
case class Ende(version: Int) extends Packet

object Day16 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day16.txt")).getLines().toList
  def paddedBin(hex: String): String = {
    String.format("%4s", Integer.toBinaryString(Integer.valueOf(hex.toString, 16))).replace(' ', '0')
  }

  // returns (remainder, value)
  def takeLiteral(binary: String, value: String): (String, String) = {
    println(s"value is $value  binary: ${binary.substring(1,5)} int: ${Integer.valueOf(binary.substring(1,5), 2)}" )
    if (binary.startsWith("0")) (binary.substring(5), value + Integer.valueOf(binary.substring(1,5), 2))
    else takeLiteral(binary.substring(5), value + Integer.valueOf(binary.substring(1,5),2))
  }

  def parseBinary(binary: String, wanted: Int): (Packet, String) = {
    println(s"binary is $binary ${if (binary.size > 19) Integer.toHexString(Integer.valueOf(binary.substring(0, 20),2))}")
    if (binary.size < 11 || wanted == 0) {println(s"something is wrong with $binary "); (Ende(-1), "")}
    else {
      val (version, rem1) = binary.splitAt(3)
      val (operator, rem2) = rem1.splitAt(3)
      println(s"version is $version, $operator is operator")
      if (operator == "100") {
        val (binaryRemainder, literalval) = takeLiteral(rem2, "")
        (Literal(literalval, Integer.valueOf(version, 2)), binaryRemainder)
      } else {
        val next = if (rem2.startsWith("1")) 11 + 1 else 15 + 1;
        println(s"rem2 $rem2 and next$next")
        if (!rem2.startsWith("1")) { // 15 bits, number of packets
          val bitSize = Integer.valueOf(rem2.substring(1, next), 2)
          var focus = rem2.substring(next, next+bitSize)
          var argList = List[Packet]()
          var result = Literal("1", 0)
          println(s"bitsize is $bitSize")
          do {
            var (res, foc) = parseBinary(focus, 1)
            focus = foc
            //println(s"foc is $foc (${foc.size})")
            argList = argList.appended(res)
          } while (focus.size > 10)
          val op = Operator(Integer.valueOf(operator, 2).toString, Integer.valueOf(version, 2), argList)
          val rest = rem2.substring(bitSize + 1)
          println(s"returning from bitsize $bitSize")
          (op, rest)
        } else {
          val nrOfArgs = Integer.valueOf(rem2.substring(1, next), 2)
          println(s"nr of Args $nrOfArgs")
          var argList = ListBuffer[Packet]()
          var listRemainder = rem2.substring(next)
          for (i <- 1 to nrOfArgs) {
            val (packet, listR) = parseBinary(listRemainder, 1)
            argList.addOne(packet)
            listRemainder = listR
          }
          val result = Operator(Integer.valueOf(operator, 2).toString, Integer.valueOf(version, 2),argList.toList)
          println(s"result from nr args: $result")
          (result, listRemainder)
        }
      }
    }
  }
//  val thePacket = Day16.parseBinary(array(0).toList.map(hex => Day16.paddedBin(hex.toString)).mkString, 1)
//  println(thePacket)

  def run(array: List[Char]): (String, String) = {
    val thePacket = parseBinary(array.map(hex => paddedBin(hex.toString)).mkString, 1)
    (s"part1: wip", s"part2: $thePacket")
  }
  println(s"${run(array(0).toList)}")
}
