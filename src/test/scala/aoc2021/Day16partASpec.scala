package aoc2021
import org.scalatest._
import flatspec._
import matchers._

class Day16partASpec extends AnyFlatSpec with should.Matchers {

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
  def testParse(str: String): Int = {
    val l = Day16partA.parseBinary(str.toList.map(hex => Day16partA.paddedBin(hex.toString)).mkString, List[Packet2]())
    println(s"versions: ${l.map(t => t.version).mkString(", ")}")
    l.foldLeft(0)((ac, p) => p.version + ac)
  }
  //  "D2FE28 parseBinary" should "give version" in {
  //    assert(testParse("D2FE28") == 6)
  //  }
  //  "8A004A801A8002F478 parseBinary" should "give version" in {
  //    assert(testParse("8A004A801A8002F478") == 16)
  //  }
  //  "EE00D40C823060 parseBinary" should "give version" in {
  //    assert(testParse("EE00D40C823060") == 14)
  //  }
  "620080001611562C8802118E34 parseBinary" should "give version" in {
    assert(testParse("620080001611562C8802118E34") == 12)
  }
  "C0015000016115A2E0802F182340 parseBinary" should "give version" in {
    assert(testParse("C0015000016115A2E0802F182340") == 23)
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
