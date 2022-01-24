package aoc2021
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day16Spec extends AnyFlatSpec with should.Matchers {

//  "process" should "process the process" in {
//    val(vlu,inp, buf) = Day15.digest("D2FE28".toList,"", 6)
//    assert(vlu == "110100")
//    assert(inp.mkString("") == "FE28")
//    assert(buf == "10")
//    assert(Day15.header(vlu) == (6, 4))
//    val (vluvlu, inp2, buf2) = Day15.digestValue(inp, buf, "")
//    assert(vluvlu.mkString("") == "011111100101")
//    assert(Integer.valueOf(vluvlu.mkString(""), 2) == 2021)
//  }
//
//  "10011101111111100011111010110101 takeLiteral" should "work fine" in {
//    assert(Day15.takeLiteral("10011101111111100011111010110101", "") == ("111010110101", 37153.toString))
//  }
//
  def version(packet: Packet, accum: Int): Int = {
    packet match {
      case l: Literal => l.version
      case o: Operator => o.version + o.args.map(p => version(p, 1)).sum
    }
  }
  def testParse(str: String): Packet = {
    println(s"testing on $str")
    val (thePacket, slack) = Day16.parseBinary(str.toList.map(hex => Day16.paddedBin(hex.toString)).mkString, 1)
    val result = version(thePacket,1)
    println(s"$thePacket")
    thePacket
  }
  "C200B40A82 parseBinary" should "give version" in {
    assert(testParse("C200B40A82") == Operator("0",6,List(Literal("1",6), Literal("2",2))))
  }
  "04005AC33890 parseBinary" should "give version" in {
    println("0400")
    assert(testParse("04005AC33890") == Operator("1",0,List(Literal("6",5), Literal("9",3))))
  }
//  "EE00D40C823060 parseBinary" should "give version" in {
//    assert(testParse("EE00D40C823060") == 14)
//  }
 "880086C3E88112 parseBinary" should "give version" in {
    assert(testParse("880086C3E88112") == Operator("2",4,List(Literal("7",5), Literal("8",6), Literal("9",0))))
  }
  "CE00C43D881120 parseBinary" should "give version" in {
    assert(testParse("CE00C43D881120") == Operator("3",6,List(Literal("7",0), Literal("8",5), Literal("9",0))))
  }
  "9C0141080250320F1802104A08 parseBinary" should "give version" in {
    assert(testParse("9C0141080250320F1802104A08") == 31)
  }

  "A0016C880162017C3686B18A3D4780 parseBinary" should "give version" in {
    assert(testParse("A0016C880162017C3686B18A3D4780") == 31)
  }

//  "processEE" should "EE00D40C823060 process the process ops1" in {
//    //val(vlu,inp, buf) = Day15.digest("8A004A801A8002F478".toList,"", 6)
//    println(Day15.part1("EE00D40C823060".toList, "", List[Int](), 24))
//  }
//
//  "process2" should "8A004A801A8002F478 process the process ops" in {
//    //val(vlu,inp, buf) = Day15.digest("8A004A801A8002F478".toList,"", 6)
//    println(Day15.part1("8A004A801A8002F478".toList, "", List[Int](), 24))
//  }



}
