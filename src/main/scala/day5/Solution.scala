package day5

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.Source

object Solution extends App {
    //val testInput = "dabAcCaCBAcCcaDA"
    // difference : 32
    val input = Source.fromFile("src/resource/inputp5.txt").getLines()

    def check(s: String) = {
        val sb = new StringBuilder(s)

        @tailrec
        def removeReact(cur: Int): Unit = {
            if (cur >= sb.length-1) Unit
            else {
                val next = cur + 1
                val a1 = sb(cur)
                val a2 = sb(next)
                val sum = math.abs(a1 - a2)
                if (sum == 32) {
                    sb.deleteCharAt(next)
                    sb.deleteCharAt(cur)
                    //println(s"   Deleted: ${sb.toString()}")
                    if (cur == 0) removeReact(cur)
                    else removeReact(cur-1)
                } else {
                    removeReact(next)
                }
            }
        }
        removeReact(0)
        sb.length
    }

    // for part one
    //check(input.toList.head)

    // for part two
    val data = input.toList.head

    @tailrec
    def removeChar(cur: Int, a: Char, b: Char, sb: StringBuilder): Unit = {
        val len = sb.length
        if (cur >= len) Unit
        else {
            val c = sb(cur)
            if (c == a || c == b) {
                sb.deleteCharAt(cur)
                removeChar(cur, a, b, sb)
            } else {
                removeChar(cur+1, a, b, sb)
            }
        }
    }

    var res = HashMap.empty[Char, Int]

    for (i <- 'a' to 'z') {
        val upper = (i - 32).toChar
        val sb = new StringBuilder(data)
        // remove
        removeChar(0, i, upper, sb)
        val result = check(sb.toString)
        res = res.updated(i, result)
    }

    val finalResult = res.minBy(f => f._2)
    println(s"$finalResult => ${finalResult._2}")

}
