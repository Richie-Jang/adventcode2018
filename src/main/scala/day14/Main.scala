package day14

object Main extends App {

    case class Item(value: Int, index: Int)
    var q = Vector.empty[Int]

    def addValue(a: Int) = {
        q = q :+ a
    }
    def appendSum(index1: Int, index2: Int) = {
        val sum = q(index1) + q(index2)
        val list = {
            val a1 =
                if (sum >= 10) sum / 10 else 0
            val a2 = sum  % 10
            if (a1 == 0) List(a2) else List(a1,a2)
        }

        for (i <- list) {
            q = q :+ i
        }
    }

    def changePosition(prev: Int, count: Int) = {
        val s = q.size
        def loop(c: Int, cur: Int): Item = {
            val nCur =
                if (cur >= s) 0 else cur
            if (c == count) Item(q(nCur), nCur)
            else {
                loop(c+1, nCur+1)
            }
        }
        loop(1, prev+1)
    }

    def single_cycle(a1: Item, a2: Item) = {
        appendSum(a1.index, a2.index)
        val p1 = changePosition(a1.index, a1.value+1)
        val p2 = changePosition(a2.index, a2.value+1)
        p1 -> p2
    }

    def print_queue(i1: Item, i2: Item, run: Int) = {
        val list =
        q.zipWithIndex.map{ case(v,i) =>
            if (i == i1.index) s"($v)"
            else if (i == i2.index) s"[$v]"
            else s"$v"
        }
        println(s"Run $run  =>  ${list.mkString(" ")}")
    }

    val input = 320851

    def loop (i1: Item ,i2: Item, run: Int, limitLen: Int): Unit = {
        if (q.size >= limitLen) {
            val list = for (i <- input to limitLen-1) yield q(i)
            println(list.mkString(""))
        }
        else {
            val (p1, p2) = single_cycle(i1, i2)
            //print_queue(p1,p2, run)
            loop(p1, p2, run + 1, limitLen)
        }
    }

    /*
    51589 first appears after 9 recipes.
    01245 first appears after 5 recipes.
    92510 first appears after 18 recipes.
    59414 first appears after 2018 recipes.
    */

    val input_vector = {
        val arr = input.toString.toCharArray.map(i => Character.digit(i, 10))
        Vector(arr:_*)
    }

    def getScores(start: Int) = {
        var list = Vector.empty[Int]
        val a1 = start + 5
        val a2 = q.size-1
        val a3 = if (a1 < a2) a1 else a2
        for (i <- start to a3) {
            list = list :+ q(i)
        }
        list
    }

    def loop_part2(i1: Item, i2: Item, run: Int): Unit = {
        val (n1,n2) = single_cycle(i1,i2)
        val scores = getScores(run-1)

        scores == input_vector match {
            case true => println(run-1)
            case _ => loop_part2(n1,n2, run+1)
        }

    }

    println(s"Check: $input_vector")

    addValue(3)
    addValue(7)
    //loop(Item(3,0), Item(7,1), 1, input+10)
    loop_part2(Item(3,0), Item(7,1), 1)

}
