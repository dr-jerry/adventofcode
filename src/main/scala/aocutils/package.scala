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

    def inBetween(coor: Point, delta: Option[Point], result: Seq[Point]): Seq[Point] = {
      //    println(s"coor: $coor , this: $this")
      val d : Point = delta.fold{val d1 = this - coor
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
      if (coor == this) result :+ coor
      else {
        inBetween(coor + d, Some(d), result :+ coor )
      }
    }
  }


  def hello() = {
    "hello"
  }
}
