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
  def dist(that: Pos3): String = "%.4f".format(Math.sqrt((x-that.x)*(x-that.x)
    + (y-that.y)*(y-that.y) + (z-that.z)*(z-that.z))).reverse.padTo(11, '0').reverse
}

object Day19 extends App {
  var array = Source.fromInputStream(this.getClass.getResourceAsStream("day19.txt")).getLines().mkString("\n").split("\n\n")
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

   mappedObservations.keys.foreach(key => {
     println(s"$key -> ${mappedObservations(key).mkString("\n   ")}")
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
