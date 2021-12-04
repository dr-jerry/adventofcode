package aoc2020

import scala.collection.mutable
import scala.io.Source

object Twenty extends App {

  case class Piece(val id: Int, val top: String, val right: String, val bottom: String, val left: String, rotation: String = "") {
    val total = s".$top.$right.$bottom.$left"

    def contains(str: String): Boolean = {
      total.indexOf(str) != -1 || total.indexOf(str.reverse) != -1
    }
  }

  val lines = Source.fromInputStream(this.getClass.getResourceAsStream("aoc2020/twenty-test.txt")).getLines().toList

  def toPieces(lins: List[String], chunk: String): List[Piece] = {
    lins match {
      case head :: tail if (head.isEmpty) => Piece(chunk) :: toPieces(tail, "")
      case head :: tail => toPieces(tail, s"$chunk\n$head")
      case Nil => List(Piece(s"$chunk"))
    }
  }

  val pieces = toPieces(lines, "")
  println(pieces)

  object Piece {
    val tilePat = "Tile (\\d+):".r

    def apply(str: String): Piece = {
      //println("str is\n " + str.trim().replaceAll("#", "1").replaceAll("\\.", "0"))
      val strArr = str.trim().replaceAll("#", "1").replaceAll("\\.", "0").split("\n")
      println("string is " + strArr.mkString("->"))
      val tilePat(id) = strArr(0)
      val right = (1 to 10).map(i => strArr(i).charAt(9)).toArray.mkString("")
      val left = (1 to 10).map(i => strArr(i).charAt(0)).toArray.mkString("")

      implicit def spl(line: String): Array[String] = line.split("")

      println("transposed" + strArr.slice(1, 11).mkString(",") + "  \n" + strArr.slice(1, 11).transpose(l => l.split("")))
      new Piece(id.toInt, strArr(1)
        , right, strArr(10), left)
    }

    def rotate(pc: Piece): Piece = {
      Piece(pc.id, pc.left.reverse, pc.top, pc.right.reverse, pc.bottom, pc.rotation + "r")
    }

    def flipV(pc: Piece): Piece = {
      Piece(pc.id, pc.top.reverse, pc.left, pc.bottom.reverse, pc.right, pc.rotation + "v")
    }

    def flipH(pc: Piece): Piece = {
      Piece(pc.id, pc.bottom, pc.right.reverse, pc.top, pc.left.reverse, pc.rotation + "h")
    }
  }

  def addToMap(key: String, pc: Piece): Unit = {
    List(key, key.reverse).foreach(k => {
      if (sides2Pieces.contains(k)) {
        sides2Pieces(k).add(pc)
        println(s"in adding ${sides2Pieces.get(k)}")
      } else sides2Pieces.put(k, new mutable.HashSet[Piece]().addOne(pc))
    })
  }

  val sides2Pieces = mutable.Map[String, mutable.Set[Piece]]()

  pieces.foreach(pc => {
    addToMap(pc.top, pc);
    addToMap(pc.left, pc)
    addToMap(pc.bottom, pc);
    addToMap(pc.right, pc)
  })

  //  sides2Pieces.keys.map(k => {println(s"k ${k}");
  //    sides2Pieces.get(k).getOrElse(new mutable.HashSet[Piece]())}).filter(s => s.size > 1)
  //    .foreach(s => println(s.mkString(",")))

  val corners = pieces.map(pc => (pc, sides2Pieces.values.filter(s => {
    /*println(s"${pc.id} -> $s -> ${s.contains(pc)}");*/
    s.size > 1 && s.contains(pc)
  }).size)).filter(t => t._2 <= 4)
  println(corners.map(t => BigInt(t._1.id)).product)


  case class Position(self: Piece, val top: Option[Piece], val right: Option[Piece]
                      , val bottom: Option[Piece], val left: Option[Piece])

  //println(Integer.toString(Integer.reverse(210),2))
}
