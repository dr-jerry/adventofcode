package aoc2021

import aocutils._

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day11 extends App {

  var partResult = 0L
  var array = Source.fromInputStream(this.getClass.getResourceAsStream( "day11.txt")).getLines()
    .map(_.split("").map(_.toInt).toArray).toArray

  val deltas = Point.getAllDeltas()


  def step(): Unit = {
    val boundary = Point(array(0).size - 1, array.size - 1)
    var flashed = Set[Point]()
    def flash(point: Point, increment: Int = 0, prepend: Option[String]): Unit = {
      //println(s"${prepend.getOrElse("")} point is  + $point")
      if (boundary.of(point) && !flashed.contains(point)) {
        //println(s"arra ${array(point.y)(point.x)}")
        array(point.y)(point.x) += increment
        if(array(point.y)(point.x) > 9) {
          flashed += point
          partResult += 1;
          //println(s"${prepend.getOrElse("")} flash on $point ${flashed.mkString(",")}")
          deltas.foreach(dlt => flash(point + dlt, 1, Some(prepend.fold("  ")(str => "  " + str))))
        }
    }}
    for(y <- 0 to boundary.y;x <- 0 to boundary.x) {
      array(y)(x) += 1
      flash(Point(x,y), 0, None)
    }
    flashed.foreach(pt => array(pt.y)(pt.x) = 0)
  }

  var finished = false
  var i = 1;
  while (!finished) {
    //println("i " + i)
    step()
  //array.foreach(l => println(l.mkString(",")))
    if (array.forall(row => row.forall( _ == 0))) {println(s"synchronized + $i");finished = true}
    i+=1
    println(s"$i ended " + partResult)
  }
 }
