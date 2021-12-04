package aoc2020

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable.ListBuffer
import scala.io.Source

object TwentyFour extends App {
  val file = "aoc2020/twentyfour.txt"

  case class Dir(val d: String, val dx: Int, val dy: Int)

  case class Tile(x: Int, y: Int, black: Boolean = false)

  val coorPattern = "\\((-?\\d+),(-?\\d+)\\)".r


  // val coorPattern = "\\((-?\\d+),(-?\\d+)\\)".r
  val allDirs = List(Dir("se", -1, 1), Dir("sw", 1, 1)
    , Dir("nw", 1, -1), Dir("ne", -1, -1), Dir("e", -2, 0), Dir("w", 2, 0))

  val dir2Coor = allDirs.map(d => d.d -> d).toMap
  println(dir2Coor)

  def display(tiles: Map[(Int, Int), Tile], fileIndex: Int) = {
    var output: ListBuffer[Array[Char]] = ListBuffer()

    val cx = 200
    val cy = 200
    (0 to cy).foreach(i => output += (" " * cx).toCharArray())
    tiles.values.foreach(tl => output(cy / 2 + tl.y)(cx / 2 + tl.x) = if (tl.black) '*' else 'O')
    output(cy / 2)(cx / 2) = '+'
    val outFile = new File(s"/Users/jeroendijkmeijer/dev/aoc2020/file-output/tile-${fileIndex}.txt")
    val bw = new BufferedWriter(new FileWriter(outFile))
    (0 to cy).foreach(i => {
      bw.write(output(i).mkString("") + "\n")
    })
    bw.close()
  }

  def modifyLine(list: List[Dir], line: String): List[(Int, Int)] = {
    list match {
      case head :: tail => modifyLine(tail, line.replaceAll(head.d, s" (${head.dx},${head.dy})"))
      case _ => line.trim.split(" ").map(c => {
        val coorPattern(x, y) = c
        (x.toInt, y.toInt)
      }).toList
    }
  }

  def toTile(list: List[(Int, Int)]): (Int, Int) = {
    list.foldLeft((0, 0)) { (acc, tup) => (acc._1 + tup._1, acc._2 + tup._2) }
  }

  def tilePattern(tiles: List[(Int, Int)], result: Map[(Int, Int), Tile]): Map[(Int, Int), Tile] = {
    tiles match {
      case coor :: tail => {
        val kv = result.get(coor).fold(coor -> Tile(coor._1, coor._2, true))(t => coor -> t.copy(black = !t.black))
        tilePattern(tail, result + kv)
      }
      case _ => result
    }
  }

  List("nwwswee", "esew").foreach(s => {
    println(s"first test $s: ${modifyLine(allDirs, s)} -> ${toTile(modifyLine(allDirs, s))}")
  })
  val lines = Source.fromInputStream(this.getClass().getResourceAsStream(file)).getLines().toList.map(line => {
    modifyLine(allDirs, line)
  })

  val tiles = tilePattern(lines.map(toTile(_)), Map())
  //lines.foreach(l => { val t = toTile(l);println(s"$l -> $t -> ${tiles(t)}")})
  println(s"total = ${tiles.values.count(t => t.black)}")


  def flip(orig: Map[(Int, Int), Tile], result: Map[(Int, Int), Tile]): Map[(Int, Int), Tile] = {
    def countCreate(focusTile: Tile, count: Int, dirList: List[Dir]): Option[Tile] = {
      dirList match {
        case head :: tail => {
          val dPos = (focusTile.x + head.dx, focusTile.y + head.dy)
          orig.get(dPos)
            .fold(countCreate(focusTile, count, tail))(fromMap => {
              countCreate(focusTile, count + (if (fromMap.black) 1 else 0), tail)
            })
        }
        case _ if (focusTile.black && (count == 0 || count > 2)) => {
          Some(focusTile.copy(black = false))
        }
        case _ if (!focusTile.black && count == 2) => {
          Some(focusTile.copy(black = true))
        }
        case _ => {
          None
        }
      }
    }

    val modified: List[Tile] = orig.values.filter(tile => tile.black).map(tl => {
      val countsAndNews: List[Option[Tile]] = (Dir("ignre", 0, 0) :: allDirs).map(pos => {
        val dPos = (tl.x + pos.dx, tl.y + pos.dy)
        orig.get(dPos).fold({
          val newTile = Tile(dPos._1, dPos._2)
          countCreate(newTile, 0, allDirs)
        })(fromMap => {
          countCreate(fromMap, 0, allDirs)
        })
      })
      countsAndNews.flatten
    }).flatten.toList
    val newMap = modified.map(tile => (tile.x, tile.y) -> tile).toMap
    orig ++ newMap
  }

  def dayFlip(tiles: Map[(Int, Int), Tile], day: Int): BigInt = {
    if (day == 0) tiles.values.count(tile => tile.black)
    else {
      println(s"on day $day ${tiles.values.count(tile => tile.black)}")
      display(tiles, day)
      dayFlip(flip(tiles, Map()), day - 1)
    }
  }

  println(s"dayFlip ${dayFlip(tiles, 100)}")
  display(tiles, 1)
  display(flip(tiles, Map()), 2)
  println(flip(tiles, Map()).values.count(t => t.black))
}
