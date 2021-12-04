package aoc2020

import scala.io.Source

object Four extends App {
  val lineIter = Source.fromInputStream(this.getClass.getResourceAsStream("four.txt")).getLines().toList
  //val headers = "byr,iyr,eyr,hgt,hcl,ecl,pid,cid".split(",").toList
  val headers = "byr,iyr,eyr,hgt,hcl,ecl,pid".split(",").toList

  case class Height(unit: String, value: Int) {
    def isValid(): Boolean = {
      if (unit == "cm") {
        value >= 150 && value <= 193
      } else if (unit == "in") {
        value >= 59 && value <= 76
      } else false
    }
  }

  case class Iden(val byr: Int, iyr: Int, eyr: Int, hgt: Height, hcl: String, ecl: String, pid: String) {
    def isValid(): Boolean = {
      byr >= 1920 && byr <= 2002 &&
        iyr >= 2010 && iyr <= 2020 &&
        eyr >= 2020 && eyr <= 2030 &&
        hgt.isValid() &&
        hcl.matches("#[0-9a-z]{6}") &&
        ":amb:blu:brn:gry:grn:hzl:oth:".indexOf(s":$ecl:") > -1 &&
        pid.matches("\\d{9}")
    }
  }

  object Iden {
    def apply(txt: String): Option[Iden] = {
      val h2v: Map[String, String] = txt.split(" ").map(kv => {
        val a = kv.split(":"); a(0) -> a(1)
      }).toMap

      try {
        val hgt = h2v("hgt")
        //012in

        Some(Iden(h2v("byr").toInt, h2v("iyr").toInt, h2v("eyr").toInt
          , Height(hgt.substring(hgt.length - 2), hgt.substring(0, hgt.length - 2).toInt)
          , h2v("hcl"), h2v("ecl"), h2v("pid")))
      } catch {
        case ex: Throwable => {
          println(s"failure for $txt"); None
        }
      }
    }
  }

  def process(lines: List[String], acum: String): List[String] = {
    lines match {
      case l :: tail if (l.nonEmpty) => process(tail, s"$l $acum")
      case Nil => List(acum)
      case "" :: tail => acum :: process(tail, "")
    }
  }

  println("expect false")
  println(Iden("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926").fold(true)(_.isValid))
  println(Iden("iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946").fold(true)(_.isValid))
  println(Iden("hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277").fold(true)(_.isValid))
  println(Iden("hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007").fold(true)(_.isValid))
  println("expect true")
  println(Iden("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f").fold(false)(_.isValid))
  println(Iden("eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm").fold(false)(_.isValid))
  println(Iden("hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022").fold(false)(_.isValid))
  println(Iden("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719").fold(false)(_.isValid))
  println("end")

  val filtered = process(lineIter, "").filter(ch => headers.forall(hdr => ch.indexOf(s"$hdr:") != -1))
  println(filtered.size)
  val idens = filtered.map(ch => Iden(ch))
  println(filtered.size)
  println(s"total ${idens.filter(_.fold(false)(_.isValid)).size}")
  println(idens)

}
