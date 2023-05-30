package day13

import scala.collection.immutable.TreeSet
import scala.io.Source

object Solution extends App {

    trait Direction
    case object L extends Direction
    case object R extends Direction
    case object T extends Direction
    case object B extends Direction
    object Direction {
        def turn_left(d: Direction): Direction = {
            d match {
                case L => B
                case R => T
                case T => L
                case B => R
            }
        }

        def turn_right(d: Direction): Direction = {
            d match {
                case L => T
                case R => B
                case T => R
                case B => L
            }
        }

        def meet_intersection(d: Direction, count: Int) = {
            count match {
                case 1 => turn_left(d)
                case 2 => d
                case 3 => turn_right(d)
            }
        }
    }

    case class Cart(x: Int, y: Int, direction: Direction, num_cross_pass: Int, indexer: Int)

    object Cart {
        def create_by_sign(x: Int, y: Int, sign: Char, index: Int): Cart = {
            val dir =
                sign match {
                    case '^' => T
                    case '<' => L
                    case '>' => R
                    case 'v' => B
                }
            Cart(x,y,dir,0, index)
        }
    }

    val test =
        """/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/"""

    def create_array(strs: Array[String]) = {
        strs.map(f => f.toCharArray)
    }

    //test case
    //val input = create_array(test.split("\r\n"))

    val input_long = """src/resource/inputp13.txt"""
    val input = create_array(Source.fromFile(input_long).getLines().toArray)

    val start_carts = {
        var carts = Vector.empty[Cart]
        for {
            y <- input.indices
            x <- input(y).indices
        } {
            val g = input(y)(x)
            g match {
                case '^' | 'v' =>
                    input(y)(x) = '|'
                    carts = carts :+ Cart.create_by_sign(x,y,g, carts.size+1)
                case '<' | '>' =>
                    input(y)(x) = '-'
                    carts = carts :+ Cart.create_by_sign(x,y,g, carts.size+1)
                case _ => ()
            }
        }
        carts
    }

    def print_array(carts: Vector[Cart]): Unit = {
        def get_sign(c: Cart) = {
            c.direction match {
                case L => '<'
                case R => '>'
                case T => '^'
                case B => 'v'
            }
        }

        def check_cart_position(x: Int, y: Int) = {
            carts.find(p => p.x == x && p.y == y)
        }

        for (y <- input.indices) {
            for (x <- input(y).indices) {
                val cart_option = check_cart_position(x,y)
                cart_option match {
                    case Some(a) => print(get_sign(a))
                    case None => print(input(y)(x))
                }
            }
            println("")
        }
    }

    def move_cart_oneStep(cart: Cart): Cart = {
        def increase_num_cross_pass(cur: Int) = {
            val c = cur + 1
            val d = c % 3
            if (d == 0) 3 else d
        }
        val (nx,ny) = {
            cart.direction match {
                case L => (cart.x-1, cart.y)
                case R => (cart.x+1, cart.y)
                case T => (cart.x, cart.y-1)
                case B => (cart.x, cart.y+1)
            }
        }
        val next_sign = input(ny)(nx)
        next_sign match {
            case '+' =>
                val new_num_cross = increase_num_cross_pass(cart.num_cross_pass)
                val new_direction = Direction.meet_intersection(cart.direction, new_num_cross)
                Cart(nx,ny,new_direction,new_num_cross,cart.indexer)
            case '/' if cart.direction == L || cart.direction == R =>
                Cart(nx,ny,Direction.turn_left(cart.direction),cart.num_cross_pass,cart.indexer)
            case '/' =>
                Cart(nx,ny,Direction.turn_right(cart.direction),cart.num_cross_pass,cart.indexer)
            case '\\' if cart.direction == L || cart.direction == R =>
                Cart(nx,ny,Direction.turn_right(cart.direction),cart.num_cross_pass,cart.indexer)
            case '\\' =>
                Cart(nx,ny,Direction.turn_left(cart.direction),cart.num_cross_pass,cart.indexer)
            case '|' | '-' =>
                cart.copy(x=nx,y=ny)
            case _ => throw new Exception(s"$next_sign is reached.")
        }
    }

    var print_line_count = 3000

    def debug_print_array(carts: Vector[Cart]) = {
        def get_sign(c: Cart) = {
            c.direction match {
                case L => '<'
                case R => '>'
                case T => '^'
                case B => 'v'
            }
        }

        def check_cart_position(x: Int, y: Int) = {
            carts.find(p => p.x == x && p.y == y)
        }

        // x: 125 ~ 133
        // y: 112 ~ 120

        for (y <- Range(112,120)) {
            for (x <- Range(125,133)) {
                val cart_option = check_cart_position(x,y)
                cart_option match {
                    case Some(a) => print(get_sign(a))
                    case None => print(input(y)(x))
                }
            }
            println("")
        }
    }

    def game_loop(sCarts: Vector[Cart]): Unit = {

        def collisionCheeck (a1: Cart, a2: Cart) = {
            a1.x == a2.x && a1.y == a2.y
        }

        def loop(carts: Vector[Cart], doneCarts: Vector[Cart]): Vector[Cart] = {
            if (carts.isEmpty) doneCarts
            else {
                val h = move_cart_oneStep(carts.head)
                val t = carts.tail
                // 0: no, 1: carts, 2: doneCarts
                val findCollision_xy = {
                    val f1 = t.find(collisionCheeck(_, h))
                    val f2 = doneCarts.find(collisionCheeck(_, h))
                    if (f1.isDefined) 1 else if (f2.isDefined) 2 else 0
                }
                var n_doneCarts = doneCarts
                var n_carts = t
                findCollision_xy match {
                    case 2 =>
                        n_doneCarts = doneCarts.filterNot(p => p.x == h.x && p.y == h.y)
                    case 1 =>
                        n_carts = t.filterNot(p => p.x == h.x && p.y == h.y)
                    case _ =>
                        n_doneCarts = n_doneCarts :+ h
                }
                loop(n_carts, n_doneCarts)
            }
        }

        def check_goal (carts: Vector[Cart]): Unit = {
            if (carts.size == 1) {
                println(s"Last : ${carts.head}")
            } else {
                val n = loop(carts, Vector.empty)
                check_goal(n)
            }
        }

        check_goal(start_carts)
    }


    // start
    game_loop(start_carts)

    println("Program is done")

}
