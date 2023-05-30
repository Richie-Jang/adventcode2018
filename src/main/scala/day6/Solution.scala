package day6

import scala.collection.immutable.{HashMap, TreeMap, TreeSet}
import scala.io.Source

object Solution extends App {

    /*val test =
        """
          |1, 1
          |1, 6
          |8, 3
          |3, 4
          |5, 5
          |8, 9
        """.stripMargin.trim()

    val points = {
        test.split("\n").map{ s =>
            val v = s.split(",")
            v(0).toInt -> v(1).trim().toInt
        }
    }

    */

    val inputPath = "src/resource/inputp6.txt"
    val points = {
        val f = Source.fromFile(inputPath).getLines
        val res =
            for (i <- f) yield {
                val ss = i.split(",").map(s => s.trim())
                ss(0).toInt -> ss(1).toInt
            }
        res.toList
    }


    // index == -1 => no point
    case class Point(x: Int, y: Int, index: Int) {
        // index -> Closet
        var overlaps = Vector.empty[(Int, Int)]
        def printInt = {
            if (index == -1) {
                var map = TreeMap.empty[Int, TreeSet[Int]]
                for (i <- overlaps) {
                    val index = i._1
                    val closet = i._2
                    val v = map.getOrElse(closet, TreeSet.empty[Int])
                    map = map.updated(closet, v + index)
                }

                val isOverlap = map.head._2.size >= 2
                if (isOverlap) -1 else map.head._2.head
            } else {
                index
            }
        }

        override def toString() = printInt.toString
    }

    val gridWidth = points.maxBy(s => s._1)._1 + 1
    val gridHeight = points.maxBy(s => s._2)._2 + 1

    println(s"Width: $gridWidth Height: $gridHeight")

    val grid = Array.tabulate(gridHeight, gridWidth){(a,b) => Point(b,a,-1)}

    points.zipWithIndex.foreach{ case (a, index) =>
        val x = a._1
        val y = a._2
        grid(y)(x) = Point(x,y,index)
    }

    /*// test
    val p = Point(1,1)
    p.overlaps = Vector((0,1), (1,2), (2,3), (3,4), (4,5))
    println(p.printChar)*/

    // check Manhattan distance
    def checkManhattanDistance (startPoint: (Int,Int), index: Int): Unit = {
        val ylen = grid.length
        val xlen = grid(0).length

        def getDistance (p1: (Int,Int), p2: (Int,Int)) = {
            math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
        }

        for (y <- 0 until ylen) {
            for (x <- 0 until xlen) {
                if (x == startPoint._1 && y == startPoint._2) Unit
                else {
                    grid(y)(x).overlaps = grid(y)(x).overlaps :+ (index, getDistance((x,y), startPoint))
                }
            }
        }
    }

    var index = 0
    for (p <- points) {
        checkManhattanDistance(p, index)
        index += 1
    }

/*    for (i <- 0 until 10) {
        println(grid(i).mkString(" "))
    }*/

    // delete infinite items
    // check size

    var infiniteItems = Set.empty[Int]
    def updateInfiniteChar(x:Int, y: Int) = {
        val c = grid(y)(x).printInt
        if (c != -2) {
            infiniteItems = infiniteItems + c
        }
    }
    for (i <- 0 until gridWidth) {
        updateInfiniteChar(i, 0)
        updateInfiniteChar(i, gridHeight-1)
    }

    for (i <- 0 until gridHeight) {
        updateInfiniteChar(0, i)
        updateInfiniteChar(gridWidth-1, i)
    }

    println(infiniteItems)

    // count items
    var map = HashMap.empty[Int, Int]
    for {
        y <- 0 until gridHeight
        x <- 0 until gridWidth
    } {
        val c = grid(y)(x).printInt
        if (c != -2 && !infiniteItems.contains(c)) {
            val v = map.getOrElse(c, 0)
            map = map.updated(c, v+1)
        }
    }

    println(map)

    // solve1
    println(map.maxBy(s => s._2))
}
