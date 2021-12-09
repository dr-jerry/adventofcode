package object aocutils {
  // Points, coordinates.
  object Point {
    def apply(xStr: String, yStr: String): Point = {
      Point(xStr.toInt, yStr.toInt)
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
      val d : Point = delta.fold{val d1 = this - that
        d1 match {
          case Point(x, y) if x < 0 && y == 0 => Point(-1,0)
          case Point(x, y) if y < 0 && x == 0 => Point(0, -1)
          case Point(x, y) if x > 0 && y == 0 => Point(1,0)
          case Point(x, y) if y > 0 && x == 0 => Point(0, 1)
          // extra for part2
          case Point(x, y) if x < 0 && y < 0 => Point(-1,-1)
          case Point(x, y) if y < 0 && x > 0 => Point(1, -1)
          case Point(x, y) if x > 0 && y > 0 => Point(1,1)
          case Point(x, y) if y > 0 && x < 0 => Point(-1, 1)
        }
      }(c => c)
      if (that == this) result :+ that
      else {
        inBetween(that + d, Some(d), result :+ that )
      }
    }
  }


  def hello() = {
    "hello"
  }
}
