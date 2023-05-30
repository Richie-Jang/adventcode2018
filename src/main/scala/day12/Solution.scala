package day12

object Solution extends App {
    val leftPadding = 5
    val rightPadding = 5

    case class State(data: String, offset: Int)

    def updatedState(state: State) = {
        val hash_start_index = state.data.indexOf('#')
        var new_data = state.data
        // check Left
        var new_left_offset = leftPadding - hash_start_index
        for (i <- 0 until new_left_offset) {
            new_data = "." + new_data
        }

        val new_right_offset = rightPadding - (state.data.length - 1 - state.data.lastIndexOf('#'))
        for (i <- 0 until new_right_offset) {
            new_data = new_data + "."
        }

        if (new_left_offset < 0) {
            new_left_offset = 0
        }
        State(new_data, state.offset+new_left_offset)
    }

    def make_condition_map (strs: Array[String]) = {
        def parseLine (s: String) = {
            val pat = """([\.\#]{5})\s=>\s([\.\#])""".r
            val pat(check, res) = s
            check -> res(0)
        }
        strs.map(parseLine).toMap
    }

    def pass_oneGeneration(state: State, conditions: Map[String,Char]) = {
        val views = {
            val data = state.data
            var res = Vector.empty[(String, Int)]
            def loop (s: Int, e: Int): Unit = {
                if (e < data.length) {
                    val ss = data.substring(s, e+1)
                    res = res :+ (ss,s)
                    loop (s+1, e+1)
                }
            }
            loop (0, 4)
            res
        }
        var res = Vector.empty[Int]
        views.foreach{ case (s,i) =>
            val f = conditions.find(p => p._1 == s)
            f match {
                case Some((_,b)) if b == '#' => res = res :+ (i+2)
                case _ => ()
            }
        }
        val sb = new StringBuilder()
        for (i <- state.data) sb.append('.')
        res.foreach(i => sb(i) = '#')
        updatedState(State(sb.toString, state.offset))
    }

    def count_pots (state: State) = {
        val indices =
            state.data.zipWithIndex.filter{ case (a,index) => a == '#'}
            .map { a => a._2 - state.offset }
        indices.sum
    }


    // start
    var test = State("##..##..#.##.###....###.###.#.#.######.#.#.#.#.##.###.####..#.###...#######.####.##...#######.##..#", 0)
    val testinput = """
                      |##... => .
                      |....# => .
                      |#.##. => .
                      |..... => .
                      |..### => .
                      |###.. => .
                      |#..#. => #
                      |##.## => .
                      |...## => #
                      |#..## => .
                      |#.### => .
                      |#.#.# => #
                      |####. => .
                      |.###. => #
                      |.##.# => .
                      |##.#. => #
                      |...#. => .
                      |.#.#. => .
                      |#...# => #
                      |##### => #
                      |..#.. => .
                      |..#.# => #
                      |..##. => .
                      |###.# => #
                      |.#### => #
                      |#.... => .
                      |.#..# => #
                      |.##.. => #
                      |#.#.. => #
                      |##..# => .
                      |.#... => #
                      |.#.## => #
                    """.stripMargin.trim.split("\r\n")
    val conditions = make_condition_map(testinput)
    println(conditions)
    test = updatedState(test)
    println(test)

    var i = 1L
    var prev = 0
    while (i <= 300) {
        test = pass_oneGeneration(test, conditions)
        val count = count_pots(test)
        println(s"$i => $count | ${count-prev}  | ${test.offset}")
        prev = count
        i += 1L
    }

    println(count_pots(test))

    val diff = 5e10.toLong - 154 + 1
    val res = BigInt(diff) * 8 + 1181
    println(res)
}
