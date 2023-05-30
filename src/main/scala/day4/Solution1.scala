package day4

import java.time.{Duration, LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import scala.collection.SortedMap
import scala.collection.immutable.{HashMap, TreeMap}
import scala.io.Source

object Solution1 extends App {

    sealed trait Action

    case class Shift(guardNum: Int) extends Action

    case object Asleep extends Action

    case object Wakeup extends Action

    case class Item(dt: LocalDateTime, act: Action)

    case class SleepTime(st: LocalDateTime, et: LocalDateTime) {
        def duration = Duration.between(st, et).toMinutes.toInt
    }

    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    def convertDateTime(s: String) = {
        LocalDateTime.parse(s, dateFormatter)
    }

    val file = "src/resource/inputp4.txt"
    //val file = "src/resource/p4test.txt"

    def parseLine(s: String) = {
        val ss = s.split(" ")
        val dstr = {
            val s = ss(0) + " " + ss(1)
            val l = s.length
            s.substring(1, l - 1)
        }
        val act = s match {
            case a if a.contains("Guard") =>
                Shift(ss(3).substring(1).toInt)
            case a if a.contains("asleep") => Asleep
            case a => Wakeup
        }
        Item(convertDateTime(dstr), act)
    }

    val itemlist =
        Source.fromFile(file).getLines()
            .map(parseLine).toList.sortWith { (s1, s2) =>
            s1.dt.isBefore(s2.dt)
        }

    var table = HashMap.empty[Int, Vector[SleepTime]]
    var cg = 0
    var ct = LocalDateTime.MIN
    for (i <- itemlist) {
        i.act match {
            case Shift(num) => cg = num
            case Asleep => ct = i.dt
            case Wakeup =>
                val v = if (table.contains(cg)) table(cg) else Vector.empty
                table = table.updated(cg, v :+ SleepTime(ct, i.dt))
        }
    }

    val longSleepGuard = {
        val v = table.mapValues(s => s.map(g => g.duration).sum)
        v.maxBy(_._2)
    }

    println(s"LongSleepGuard : ${longSleepGuard._1} TotalSleep = ${longSleepGuard._2}")

    val times = table(longSleepGuard._1).sortWith { (a, b) =>
        a.st.compareTo(b.st) < 0
    }

    def drawTable(list: Vector[SleepTime]) = {
        println("     000000000011111111112222222222333333333344444444445555555555")
        println("     012345678901234567890123456789012345678901234567890123456789")

        var map = HashMap.empty[LocalDate, Array[Char]]
        for (t <- list) {
            val tt = t.st.toLocalDate
            val value = if (map.contains(tt)) map(tt) else Array.fill[Char](60)('.')
            val stt = t.st.toLocalTime.getMinute
            val ett = t.et.toLocalTime.getMinute - 1
            for (vvv <- stt to ett) {
                value(vvv) = '#'
            }
            map = map.updated(tt, value)
        }

        val ks = map.keys.toList.sortWith((a, b) => a.isBefore(b))
        val form = DateTimeFormatter.ofPattern("MM-dd")
        val sumArr = Array.ofDim[Int](60)
        for (k <- ks) {
            println(s"${k.format(form)}${map(k).mkString("")}")
            val vv = map(k)
            for (g <- 0 until 60) {
                if (vv(g) == '#') sumArr(g) += 1
            }
        }

        println()
        println(sumArr.mkString(","))
        println(s"Most time : ${sumArr.zipWithIndex.maxBy(s => s._1)._2}")
    }

    //drawTable(times)

    // part2 ==> using table
    def calculateSleepTimesAndFindMostMinutesMax(list: Vector[SleepTime]) = {
        val slist = list.sortWith((a, b) => a.st.isBefore(b.st))
        var map = HashMap.empty[LocalDate, Array[Char]]
        for (s <- slist) {
            val t = s.st.toLocalDate
            val value = if (map.contains(t)) map(t) else Array.fill[Char](60)('.')
            val sst = s.st.toLocalTime.getMinute
            val est = s.et.toLocalTime.getMinute - 1
            for (g <- sst to est) {
                value(g) = '#'
            }
            map = map.updated(t, value)
        }
        val arrSum = Array.ofDim[Int](60)
        for (kv <- map) {
            val gg = kv._2
            for (i <- 0 until 60) {
                if (gg(i) == '#') arrSum(i) += 1
            }
        }
        arrSum.zipWithIndex.maxBy(s => s._1)
    }

    val res = table.mapValues(s => calculateSleepTimesAndFindMostMinutesMax(s))

    val r1 = res.maxBy(s => s._2._1)
    println(s"Guard ${r1._1} MaxTime: ${r1._2._1} Minutes: ${r1._2._2}")

}