package day15

import scala.collection.mutable.ListBuffer

object Part1 {

    case class CUnit(x: Int, y: Int, energy: Int, isElf: Boolean = true)
    type Grid = Array[Array[Int]]

    def createGrid (strs: Array[String]) = {
        val lb = ListBuffer.empty[CUnit]
        val grid = Array.ofDim[Int](strs.length, strs(0).length)
        for {
            y <- strs.indices
            x <- 0 until strs(y).length
        } {
            val c = strs(y)(x)
            c match {
                case '#' => grid(y)(x) = 1
                case 'G' => lb += CUnit(x,y,200,false); grid(y)(x) = 0
                case 'E' => lb += CUnit(x,y,200); grid(y)(x) = 0
                case _ => grid(y)(x) = 0
            }
        }
        grid.asInstanceOf[Grid] -> lb.toList
    }

    def sortOrderUnits (us: List[CUnit]) = us.sortWith { (a,b) =>
        if (a.y == b.y) {
            a.x < b.x
        } else {
            a.y < b.y
        }
    }

    val list = List(
        CUnit(4, 1, 100),
        CUnit(2, 1, 100, false),
        CUnit(1, 2, 100, true),
        CUnit(5, 2, 100, false),
        CUnit(2, 3, 100, false),
        CUnit(3, 2, 100, true)
    )
    /*
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########

#########
#.G...G.#
#...G...#
#...E..G#
#.G.....#
#.......#
#G..G..G#
#.......#
#########
     */
    def fixedGrid(list: List[CUnit], grid: Grid, sameUnit: Boolean):Unit = {
        val update_value = if (sameUnit) 2 else 3
        list match {
            case h :: t =>
                grid(h.y)(h.x) = update_value
                fixedGrid(t, grid, sameUnit)
            case Nil => Unit
        }
    }
    def clearGrid(grid: Grid) = {
        for {
            y <- grid.indices
            x <- grid(y).indices
        } if (grid(y)(x) >= 2) grid(y)(x) = 0
    }
/*
In range:     Nearest:      Chosen:       Distance:     Step:
#######       #######       #######       #######       #######
#.E...#       #.E...#       #.E...#       #4E212#       #..E..#
#...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
#..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
#######       #######       #######       #######       #######
 */
    def get4Directions (x: Int, y: Int, lx: Int, ly: Int) = {
        List((x-1,y), (x+1, y), (x, y-1), (x, y+1)).filter{case(x,y) =>
                x >= 0 && x < lx && y >= 0 && y < ly
        }
    }

    def measureDistance (oxy: (Int,Int), txy: (Int,Int), grid: Grid) = {

    }

    def nearestGrid(grid: Grid, us: List[CUnit], ox: Int, oy: Int) = {
        var tmp = Vector.empty[(Int,Int)]
        for (i <- us) {
            val d4 = get4Directions(i.x, i.y, grid(0).length, grid.length).filter(f => grid(f._2)(f._1) == 0)
            d4.foreach{xy => tmp = tmp :+ xy}
        }

    }

    def findClosetUnit (cur: CUnit, us: List[CUnit], grid: Grid) = {
        val curtype = cur.isElf
        val sames = us.filter(f => f.isElf == curtype && f != cur)
        val competitors = us.filter(f => f.isElf != curtype)
        grid(cur.y)(cur.x) = 9
        fixedGrid(sames, grid, true)
        fixedGrid(competitors, grid, false)
        nearestGrid(grid, competitors, cur.x, cur.y)
        print_grid2(grid)
    }

    def print_grid2 (grid: Grid) = {
        grid.foreach{ x => println(x.mkString("")) }
    }

    def print_grid(grid: Grid, units: List[CUnit]) = {
        // elf : 2, Goblin : 3
        val map = units.map{ c => ((c.x, c.y),if (c.isElf) 2 else 3)}.toMap
        for (y <- 0 until grid.length) {
            for (x <- 0 until grid(y).length) {
                var cc = '.'
                if (map.contains((x, y))) {
                    cc = if (map((x, y)) == 2) 'E' else 'G'
                } else {
                    grid(y)(x) match {
                        case 1 => cc = '#'
                        case _ => cc = '.'
                    }
                }
                print(cc)
            }
            println()
        }
        println()
    }


    def main(args: Array[String]): Unit = {
        val test = """#########
                     |#G..G..G#
                     |#.......#
                     |#.......#
                     |#G..E..G#
                     |#.......#
                     |#.......#
                     |#G..G..G#
                     |#########""".stripMargin.split("\r\n")
        val (grid, units) = createGrid(test)
        print_grid(grid, units)

        findClosetUnit(units.head, units, grid)
    }

}
