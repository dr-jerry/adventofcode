import com.twitter.finagle.http._
import com.twitter.finagle.{Failure, Http, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.util.{Await, Future}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

package object aocutils {
  def points2D (points: Seq[Point], char: Char = 'X'): String = {
    val min = points.foldLeft(Point(10000, 10000))((min, pt) => {
      Point(math.min(pt.x, min.x), math.min(pt.y, min.y))
    })
    val max = points.foldLeft(Point(-10000, -10000))((max, pt) => {

      Point(math.max(pt.x, max.x), math.max(pt.y, max.y))
    })
//    println(s" max $max min: $min ")
    val buff = Array.fill(max.y - min.y +1, max.x - min.x+1)('.')
    //buff(2)(3) = 'x'
    points.zipWithIndex.foreach( tup => {buff(tup._1.y - min.y)(tup._1.x - min.x) = ('0' +tup._2).toChar})
    buff.map(l => l.mkString).mkString("\n")
  }

  def transpose(rows: List[String]): List[String] = {
    val max = rows.map(line => line.size).max
    // make all rows are equal length, transpose, select the propriate lines
    rows.map(l => l.padTo(max, ' ')).transpose
      .map(_.toString.trim).toList
  }
  def getFromAOC(year: Int, day: Int): String = {
    val cookieContents = Source.fromInputStream(this.getClass.getResourceAsStream("sessionID")).getLines().toList(0)
    val client: Service[Request, Response] = Http.client.withTlsWithoutValidation.newService("www.adventofcode.com:443")

    val request = Request(Method.Get, s"https://adventofcode/$year/day/$day/input")
    // extract cookie contents from browserrequest, store in file aocutils/sessionID
    request.headerMap.add("cookie", cookieContents)
    val response: Future[Response] = client(request)
    val result = Await.result(response).getContentString()
    result
  }

  // Points, coordinates.
  object Point {
    val commaPattern= "(\\d+) ?, ?(\\d+)".r
    def apply(str: String): Point = {
      val commaPattern(x,y) = str
      Point(x.toInt,y.toInt)
    }
    def apply(xStr: String, yStr: String): Point = {
      Point(xStr.toInt, yStr.toInt)
    }
    def getAllDeltas(): List[Point] ={
      List(Point(1, 0), Point(1, 1), Point(0, 1), Point(-1, 1), Point(-1, 0), Point(-1, -1), Point(0, -1), Point(1, -1))
    }
    def SquaredPoints(): List[Point] = {
      List(Point(1, 0), Point(-1, 0), Point(0, -1), Point(0, 1))
    }
  }
  case class Line(s: Point, e: Point)
  case class Point(val x: Int, y: Int) {
    def +(coor: Point): Point = {
      Point(x + coor.x, y + coor.y);
    }

    def -(coor: Point): Point = {
      //    println(s"minning this $this, that: $coor")
      Point(x - coor.x, y - coor.y)
    }

    def *(v: Int): Point = {
      Point(v * x, v * y)
    }

    // return if this in boundary.
    def in(boundary: Point): Boolean = {
      (x >= 0) && (y >= 0) && (x <= boundary.x) && (y <= boundary.y)
    }

    // return whether pt within (boundary) boundary.of(pt)
    def of(pt: Point): Boolean = {
      (pt.x >= 0) && (pt.y >= 0) && (pt.x <= x) && (pt.y <= y)
    }

    // return all points horirzontal / vertical between this & that
    def inBetween(that: Point, delta: Option[Point], result: Seq[Point]): Seq[Point] = {
      //    println(s"coor: $coor , this: $this")
      val d: Point = delta.fold {
        val d1 = this - that
        d1 match {
          case Point(x, y) if x < 0 && y == 0 => Point(-1, 0)
          case Point(x, y) if y < 0 && x == 0 => Point(0, -1)
          case Point(x, y) if x > 0 && y == 0 => Point(1, 0)
          case Point(x, y) if y > 0 && x == 0 => Point(0, 1)
          // extra for part2
          case Point(x, y) if x < 0 && y < 0 => Point(-1, -1)
          case Point(x, y) if y < 0 && x > 0 => Point(1, -1)
          case Point(x, y) if x > 0 && y > 0 => Point(1, 1)
          case Point(x, y) if y > 0 && x < 0 => Point(-1, 1)
        }
      }(c => c)
      if (that == this) result :+ that
      else {
        inBetween(that + d, Some(d), result :+ that)
      }
    }

    def adjacent(that: Point): Boolean = {
      (this - that + Point(1, 1)).in(Point(2, 2))
    }
  }

}
