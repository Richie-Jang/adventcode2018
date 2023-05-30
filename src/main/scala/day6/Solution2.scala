package day6

import scala.io.Source

object Solution2 extends App {

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
    }*/

    val inputPath = "src/resource/inputp6.txt"
    val points = {
        val f = Source.fromFile(inputPath).getLines
        val res =
        for (i <- f) yield {
            val ss = i.split(",\\s")
            ss(0).toInt -> ss(1).toInt
        }
        res.toArray
    }

    val limit = 10000
    val gridWidth = points.maxBy(s => s._1)._1 + 1
    val gridHeight = points.maxBy(s => s._2)._2 + 1
    val grid = Array.fill(gridHeight, gridWidth)(-1)

    // fillup
    points.zipWithIndex.foreach{ case ((x,y),index) =>
        grid(y)(x) = index
    }

    //calculate distance

    def calculateMahattanDistance(p1: (Int,Int), p2: (Int,Int)) = {
        math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
    }

    var candidates = List.empty[(Int,Int)]
    for {
        y <- 0 until gridHeight
        x <- 0 until gridWidth
    } {
        val curp = x -> y
        val dist = points.map(s => calculateMahattanDistance(curp, s)).sum
        if (dist < limit) {
            candidates = curp::candidates
        }
    }

    candidates.foreach {c =>
        grid(c._2)(c._1) = -2
    }

    /*// print grid
    for (i <- 0 until gridHeight) {
        println(grid(i).map(s => f"$s%2d").mkString("  "))
    }
    */

    var res = 0
    for {
        y <- 0 until gridHeight
        x <- 0 until gridWidth
    } {
        if (grid(y)(x) == -2) res += 1
    }

    println(res)
}
