package aoc2023
import scala.io.Source
case class AOCRange(start: Long, end: Long) {
  def within(x: Long): Boolean = {
    x >= start && x <= end
  }
  // ..****....**..***......************..
  // .....yyyyyy
  //      122223
  def doSplit(ranges: List[AOCRange], result: List[AOCRange]): List[AOCRange] = {
    ranges match {
      case Nil => result :+ this
      case head :: tail => {
        if (end < head.start) List(this)
        else if (start > head.end) doSplit(tail, result)
        else if (within(head.start)) {
          if (within(head.end)) new AOCRange(head.end+1, end).doSplit(ranges, result ++ List(new AOCRange(this.start, head.start-1),new AOCRange(head.start, head.end)))
          else result :+ this
        } else {
          // head.start < this.start
          if (head.end >= end) result :+ new AOCRange(start, end)
          else new AOCRange(head.end+1, end).doSplit(tail, result ++ List(new AOCRange(start, head.end)))
        }
      }
    }
  }
}

case class MapRange(destRange: AOCRange, sourceRange: AOCRange) {
  def mapMe(x: Long) : Option[Long] = {
    if (sourceRange.within(x)) Some(destRange.start + x-sourceRange.start) else None
  }
 // def mapRange(sourceRange: AOCRange): List[AOCRange]
}


object MapRange {
  val divider = 1;
  implicit object AOCRangeOrder extends math.Ordering[AOCRange] {
    def compare(a: AOCRange, b: AOCRange) = {
      a.start.compareTo(b.start)
    }

  }

  implicit object MapRangeOrder extends math.Ordering[MapRange] {
    def compare(a: MapRange, b: MapRange) = {
      a.sourceRange.start.compareTo(b.sourceRange.start)
    }
  }


    def apply(str: String): MapRange  = {
    val l = str.split(" +")
    new MapRange(AOCRange(l(0).toLong/divider, (l(0).toLong + l(2).toLong)/divider)
      ,AOCRange(l(1).toLong/divider, (l(1).toLong + l(2).toLong)/divider))
  }

}

object Day05 extends App {

  val real = Source.fromInputStream(this.getClass.getResourceAsStream("day05-test.txt")).getLines().mkString(";")

  //val real = aocutils.getFromAOC(2023, 5).split("\n").toList.mkString(";")
  val s"seeds: $seeds;;$rest" = real

   val mp = rest.split(";;").zipWithIndex.map(t => {
    val l = t._1.split(";")
    (s"${t._2} ${l.head}" -> (l.tail.map(MapRange(_)).sorted))
  }).toMap

  val result = seeds.split(" +").map(seed => mp.keys.toList.sorted.foldLeft(seed.toLong)((r, source) => {
    val dests = mp.get(source).get.map(mr => mr.mapMe(r)).flatten
 //   println(dests.mkString(","))
    if(dests.size == 0) r else { if (dests.size > 1) {println(s"problem: $r $source");dests(0)} else dests(0)}
  })).min
  println("part1 result " + result)


  import MapRange._
  val seedRanges = seeds.split(" ").sliding(2,2).map(l => AOCRange(l(0).toLong/MapRange.divider, (l(1).toLong + l(0).toLong)/MapRange.divider)).toList.sorted
  println(seedRanges)
  mp.get("0 seed-to-soil map:").get.foreach(mr => println(s"${mr.sourceRange.start/MapRange.divider} ${mr.sourceRange.end/MapRange.divider}"))
  val soilRanges = mp.get("0 seed-to-soil map:").get.toList
  seedRanges.map(seed => println(s"$seed -> ${seed.doSplit(soilRanges.map(mr => mr.sourceRange), List()).mkString(";")}"))

}
