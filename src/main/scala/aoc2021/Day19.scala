package aoc2021

import aoc2021.Day06.array

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Pos3 {
  def apply (x: String, y: String, z: String, scanner: String, index: Int): Pos3 = {
    Pos3(Integer.valueOf(x), Integer.valueOf(y), Integer.valueOf(z), Integer.valueOf(scanner), index)
  }
}
case class Pos3 (var x: Int, var y: Int, var z: Int, scanner: Int, index: Int) {
  def dist(that: Pos3): Double = Math.sqrt((x-that.x)*(x-that.x)
    + (y-that.y)*(y-that.y) + (z-that.z)*(z-that.z))
  def diff(that: Pos3): Pos3 = Pos3(this.x - that.x, this.y - that.y, this.z - that.z, this.scanner, this.index)
  def pos3Equal(that: Pos3) = {
    (this.x == that.x && this.y == that.y && this.z == that.z)
  }
  def rotate(ax: Char, theta: Double): Pos3 = {
    // if theta is Int your sin/cos calculation will go sout.
    // 2 rotations: one for direction: up,dow, north, south, east, west.
    // one for the rotation.
    if (theta == 0) this
    else {
      val (sin, cos) = (Math.sin(theta / 180 * Math.PI).toInt, Math.cos(theta / 180 * Math.PI).toInt)
      //println(s"$theta: sin is $sin ${Math.sin(theta.toDouble / 180 * Math.PI)}, cos $cos (${Math.cos(theta.toDouble / 180 * Math.PI)})")
      //println(s"$theta: sin is $sin, cos $cos)")
      ax match {
        //        Z-axis would be
        //
        //          |cos θ   −sin θ   0| |x|   |x cos θ − y sin θ|   |x'|
        //        |sin θ    cos θ   0| |y| = |x sin θ + y cos θ| = |y'|
        //        |  0       0      1| |z|   |        z        |   |z'|
        case 'Z' => Pos3(this.x * cos - this.y * sin, this.x * sin + this.y * cos, this.z, this.scanner, this.index)
        //        around the Y-axis would be
        //
        //        | cos θ    0   sin θ| |x|   | x cos θ + z sin θ|   |x'|
        //        |   0      1       0| |y| = |         y        | = |y'|
        //        |−sin θ    0   cos θ| |z|   |−x sin θ + z cos θ|   |z'|
        case 'Y' => Pos3(this.x * cos + this.z * sin, this.y, -this.x * sin + this.z * cos, this.scanner, this.index)
        //        around the X-axis would be
        //
        //        |1     0           0| |x|   |        x        |   |x'|
        //        |0   cos θ    −sin θ| |y| = |y cos θ − z sin θ| = |y'|
        //        |0   sin θ     cos θ| |z|   |y sin θ + z cos θ|   |z'|
        case 'X' => Pos3(this.x, this.y * cos - this.z * sin, this.y * sin + this.z * cos, this.scanner, this.index)
      }
    }
  }

}

object Day19 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day19b.txt")).getLines().mkString("\n").split("\n\n")
  val pos3re = "(-?\\d+),(-?\\d+),(-?\\d+)".r
  val scannere = "--- scanner (\\d+) ---".r

  val field = mutable.HashMap[String, mutable.Set[(Pos3, Pos3)]]()
  val sharedObversation = "id"

  array.map(chunck => {
    val scans = chunck.split("\n")
    val scannere(scanId) = scans.head
    val coors = scans.tail.zipWithIndex.map(tup => {
      val pos3re(x, y, z) = tup._1
      Pos3(x, y, z, scanId, tup._2)
    }).combinations(2).foreach(comb => {
      val dist = s"${comb(0).dist(comb(1))}"
      field += (dist -> (field.getOrElse(dist, mutable.Set()) += ((comb(0), comb(1)))))
      //println(s"${comb(0).dist(comb(1))} ${comb(0).id} <-> ${comb(1).id}")
    })
  })

  val c2c = (for {sets <- field.values
       scan2 <- (0 to array.size).combinations(2)
       one <- sets.find(tup => tup._1.scanner == scan2(0))
       two <- sets.find(tup => tup._1.scanner == scan2(1))
       if (one != None && two != None)
  } yield (s"${scan2(0)}-${scan2(1)}", (one._1,two._1),(one._1,two._2),(one._2,two._1),(one._2,two._2))).groupBy(tup => tup._1)

   val mappedObservations = c2c.keys.map(key => {
    val matchs =c2c(key).flatMap(tup=> List(tup._2, tup._3, tup._4, tup._5)).groupBy(identity).view.mapValues(l => l.size).toMap
    key -> matchs.keys.filter(pair => matchs(pair) > 1)}).toMap

   val axis = List('X', 'Y', 'Z')
   val angles = List(0, 90, 180, 270)
   val rotations = for {
     ax <- axis
     an <- angles
     sec <- for {
       ax2 <- axis
       a2 <- angles
       if (ax2 != ax && (ax == 'X' || ax == 'Y' && ax2 == 'Z')) } yield (ax2, a2)
   } yield ((ax, an), sec)

  println(s"rotations: $rotations")
   mappedObservations.keys.foreach(key => {
     val theList = mappedObservations(key)
     val normalizedList = theList.map(coor_pair => (coor_pair._1.diff(theList.head._1), coor_pair._2.diff(theList.head._2)))
     val r = rotations.find(rot => {
       normalizedList.forall(p => {
         val trial = p._2.rotate(rot._1._1, rot._1._2).rotate(rot._2._1, rot._2._2)
         //println(s"$rot: ${p._2} -> $trial ref ${p._1}")
         trial.pos3Equal(p._1)
       })
     })
     println(s"found $key => $r")
   })




  //    if (field.get(key).get.size >= 2) {
//      field.get(key).foreach(f)
//      println(s"$key ${field.get(key)}")
//    }




//  def run(array: List[Char]): (String, String) = {
//    val thePacket = parseBinary(array.map(hex => paddedBin(hex.toString)).mkString, 1)
//    (s"part1: wip", s"part2: $thePacket")
//  }
//  println(s"${run(array(0).toList)}")
}
