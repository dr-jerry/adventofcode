package aoc2023
import scala.io.Source
object Day03 extends App {

  val bag = Map("red"  -> 12, "green" -> 13, "blue" -> 14)
  def part1(list: List[String]): Long = {
    println(list)
    val numre = "(\\d+)".r
    val notNumre = "[^\\d\\.]".r
    println(notNumre.findFirstIn("....*"))
    val result = list.sliding(3,1).toList.map(win => win.map(l => s".$l.")).map(win => {
      val matches = numre.findAllMatchIn(win(1))
      matches.filter(rm => {win.exists(line => {
//        println(s"win:$line  ${line.substring(rm.start-1, rm.end + 1)} matches: ${notNumre.matches(line.substring(rm.start-1, rm.end + 1))}")
        notNumre.findFirstIn(line.substring(rm.start-1, rm.end + 1)) match {
          case Some(s) => true
          case _ => false
        }
      })}).map(rm => rm.group(1)).map(_.toInt).toList.sum
    })
    println(s"result is ${result.sum}")
      //.foreach(l => println(s"list  + $l + ${l.size}"))

    0L
  }

  def part2(list: List[String]): Long = {
    val numre = "(\\d+)".r
    val starRe = "\\*".r
    val iterator = list.sliding(3,1)

    val result = list.sliding(3, 1).toList.map(win => win.map(l => s"...$l...")).map(win => {
      val matches = starRe.findAllMatchIn(win(1)).toList
     val stars = matches.map(rm => {
        val found = win.map(line => {
          val substr = line.substring(rm.start-4, rm.end+4)
          val subResult = numre.findAllMatchIn(substr).toList.filter(re => re.start-1 to re.end contains(4)).map(re => re.group(1).toLong)
          subResult
          })
        found.flatMap(x => x)
        })
        stars
      })
    result.flatMap(x => x).filter(l => l.size > 1).map(l => l.product).sum
  }


  //val real = Source.fromInputStream(this.getClass.getResourceAsStream("day03.txt")).getLines().toList
  val real = aocutils.getFromAOC(2023, 3).split("\n").toList
  println("part2: " + part2("." * real(0).size +: real:+ "." * real(0).size))
  //println("part2: " + part2(real))
  //
  //println(s"outcome = ${real.map(l => toNum(l)).sum}")
  //println(s"2 outcome = ${part2(real)}")

}
