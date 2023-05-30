package day8

import scala.collection.mutable
import scala.io.Source

object Solution extends App {

    val testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

    case class Node(level: (Int,Int), childCount: Int, metaCount: Int) {
        val metaDatas = Array.ofDim[Int](metaCount)
        def metaSum = metaDatas.sum
    }

    object Node {
        implicit val ord = Ordering.fromLessThan{(a: Node, b: Node) =>
            if (a.level._1 == b.level._1) {
                a.level._2 < b.level._2
            } else {
                a.level._1 < b.level._1
            }
        }
    }

    def solve1 (str: String): mutable.TreeMap[Node, Vector[Node]] = {

        val iter = str.split("\\s").toStream.map(i => i.toInt).toIterator
        val map = mutable.TreeMap.empty[Node, Vector[Node]]
        val headNode = Node((0,0), iter.next(), iter.next())
        //println(headNode)

        var metaDataAll = 0
        def loop (curNode: Node, iter: Iterator[Int]): Unit = {
            if (!map.contains(curNode)) map.put(curNode, Vector.empty)
            if (curNode.childCount == 0) {
                val m = curNode.metaCount
                for (j <- 0 until m) {
                    val cc =iter.next()
                    metaDataAll += cc
                    curNode.metaDatas(j) = cc
                }
                return
            }
            //check bigbrother child size.
            val leftSize = {
                val c = curNode.level._2
                if (c == 0) 0
                else {
                    map.filter { p => p._1.level._1 == curNode.level._1 && p._1.level._2 < c}.map(s => s._1.childCount).sum
                }
            }
            for (i <- 0 until curNode.childCount) {
                val cNode = Node((curNode.level._1+1) -> (leftSize+i), iter.next(), iter.next())
                map.update(curNode, map(curNode) :+ cNode)
                loop(cNode, iter)
            }

            for (j <- 0 until curNode.metaCount) {
                val cc = iter.next()
                metaDataAll += cc
                curNode.metaDatas(j) = cc
            }
        }

        loop(headNode, iter)

        var res = 0
        for (kv <- map) {
            res += kv._1.metaSum
            println(s"${kv._1} Sum: ${kv._1.metaSum} ==> ${kv._2}")
        }

        val res2 = map.keys.foldLeft(0){case (s,i) => i.metaSum + s}

        println(res)
        println(res2)

        println(s"MetaData All : $metaDataAll")

        map
    }

    //val map = solve1(testInput)

    val input = Source.fromFile("""src/resource/inputp8.txt""").mkString
    val map = solve1(input)


    def solve2 (map: mutable.TreeMap[Node, Vector[Node]]): Int = {

        val nMap = mutable.HashMap.empty[Node, Int]

        def goIn(cur: Node): Unit = {
            if (cur.childCount == 0) {
                nMap.update(cur, cur.metaSum)
            } else {
                var sum = 0
                val subNodes = map(cur)
                val subSize= subNodes.size
                cur.metaDatas.foreach{ index =>
                    println(s"  $cur Meta: Index $index")
                    val rindex = index-1
                    if (rindex >= subSize) sum += 0
                    else {
                        val sub = subNodes(rindex)
                        if (!nMap.contains(sub)) {
                            goIn(sub)
                        }
                        sum += nMap(sub)
                    }
                }
                nMap.update(cur, sum)
            }
        }

        goIn(map.head._1)

        nMap(map.head._1)
    }

    println(solve2(map))
}
