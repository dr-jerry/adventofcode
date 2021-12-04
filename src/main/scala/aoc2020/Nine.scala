package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Nine extends App {
  val pLength = 25
  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/nine.txt")).getLines()
    .map(n => BigInt(n)).toList

  @tailrec
  def verify(lines: List[BigInt]): BigInt = {
    val (cypher, rest) = lines.zipWithIndex.partition(tp => tp._2 < pLength)
    rest match {
      case Nil => -1
      case _ => {
        if (cypher.map(tp => tp._1).combinations(2).toList
          .exists(l => l.sum == rest.head._1))
          verify(lines.tail)
        else
          rest.head._1
      }
    }
  }

  val wrong = verify(lines)


  val arry = lines.toArray;
  (3 to 30).foreach(m => {

    (0 to arry.length - m).foreach(s => {
      val slice = arry.slice(s, m + s).sortWith(_ < _)
      //slice.foreach(println);println(s"--${slice.sum} - $wrong = ${slice.sum - wrong}");
      if (slice.sum == wrong) {
        slice.foreach(l => println(s">> $l")); println(s"->${slice(0) + slice(slice.length - 1)}")
      }
    })
  })

}
