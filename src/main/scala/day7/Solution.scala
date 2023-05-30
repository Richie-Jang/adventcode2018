package day7

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Solution extends App {

    def parseLine(s: String) = {
        val pat = """Step (\w) must be finished before step (\w) can begin\.""".r
        val pat(a,b) = s
        a(0) -> b(0)
    }

    lazy val testInput = {
        val test =
            """
              |Step C must be finished before step A can begin.
              |Step C must be finished before step F can begin.
              |Step A must be finished before step B can begin.
              |Step A must be finished before step D can begin.
              |Step B must be finished before step E can begin.
              |Step D must be finished before step E can begin.
              |Step F must be finished before step E can begin.
            """.stripMargin.trim()

        test.split("\n").map(s => parseLine(s.trim))
    }

    lazy val input = {
        val inputFile = "src/resource/inputp7.txt"
        val res =
            for (l <- Source.fromFile(inputFile).getLines) yield {
                parseLine(l)
            }
        res.toArray
    }

    case class Node(name: Char) {
        val parents = ArrayBuffer.empty[Char]
        val nexts = ArrayBuffer.empty[Char]
    }

    val graph = {
        val map = mutable.TreeMap.empty[Char, Node]
        input.foreach { case (a,b) =>
            if (!map.contains(a)) map.put(a, Node(a))
            if (!map.contains(b)) map.put(b, Node(b))
            map(b).parents += a
            map(a).nexts += b
        }
        map
    }

    val sb = new mutable.StringBuilder()
    val heads = {
        val res = mutable.SortedSet.empty[Char]
        graph.filter(p => p._2.parents.isEmpty).foreach(c =>
            res += c._1
        )
        res
    }

    println(heads)

    // part1
    def loop (buf: mutable.SortedSet[Char], finished: mutable.Set[Char]): Unit = {
        if (buf.isEmpty) Unit
        else {
            val c = buf.head
            sb.append(c)
            finished += c
            buf -= c
            for (i <- graph(c).nexts) {
                val g = graph(i)
                if (g.parents.toSet.diff(finished).isEmpty) {
                    buf += i
                }
            }
            loop(buf, finished)
        }
    }


    loop(mutable.SortedSet(heads.toSeq :_*), mutable.Set.empty)
    println(sb.toString)

    // part2

    val workerCount = 5
    case class Worker(work: Char) {
        val addTime = if (work == '.') 0 else work.toInt - 64 + 60
        var counter = 0
        def checkTime(): Unit = {
            if (work == '.') {
                // no assigned
            } else {
                counter += 1
            }
        }
        def finished() = counter >= addTime
    }

    val workers = Array.fill(workerCount)(Worker('.'))

    var timer = 0
    var buffer = mutable.SortedSet.empty[Char]

    def getAvailableWorkers = {
        var list = Vector.empty[Int]
        for (i <- 0 until workerCount) {
            if(workers(i).finished) list = list :+ i
        }
        list
    }

    var workingWorkerIndex = Array.fill(workerCount)(0)
    var doneList = Vector.empty[Char]
    buffer = buffer ++ heads


    def checkCount = {
        doneList.size == graph.size
    }

    while (!checkCount) {
        // set
        val realWorkFinishes = {
            var list = Vector.empty[Int]
            for(i <- workers.indices) {
                if (workers(i).work != '.' && workers(i).finished()) list = list :+ i
            }
            list
        }
        println(s"      RealWorkFinished : ${realWorkFinishes.mkString(", ")}")

        for {
            i <- realWorkFinishes
        } {
            val cc = workers(i).work
            doneList = doneList :+ cc
            val g = graph(cc)
            for (j <- g.nexts) {
                val gg = graph(j)
                if (gg.parents.diff(doneList).isEmpty) {
                    buffer += j
                }
            }
            workers(i) = Worker('.')
        }

        if (!checkCount) {
            val wcount = getAvailableWorkers
            println(s"      AvailableWorkers: $wcount")

            for {
                i <- wcount
                if buffer.nonEmpty
            } {
                val c = buffer.head
                workers(i) = Worker(c)
                workingWorkerIndex(i) = 1
                buffer -= c
            }

            println(s"  Buffer: $buffer")
            workers.foreach(p => p.checkTime())
            timer += 1
            println(s"      Timer: $timer")
            println(s"DoneList: $doneList")
        }
    }

    println(doneList)
    println(timer)

}
