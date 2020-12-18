object Day11 {

  case class Point (x: Int, y: Int) {
    def +(that: Point): Point = Point (x + that.x, y + that.y)
  }

  val directions = for (y <- -1 to 1; x <- -1 to 1; if x != 0 || y != 0) yield Point(x, y)

  type Seats = Vector[Vector[Option[Boolean]]]

  /*val map: Seats = 
    scala.io.Source.fromFile("input/day11.txt").getLines()
    .map{x => 
      x.toVector.map{y => 
        y match{
          case '#' => Some(true)
          case 'L' => Some(false)
          case _ => None
        }
      }
    }.toVector*/

  def inBounds(map: Seats, p: Point): Boolean = p.x >= 0 && p.x < map.head.size && p.y >= 0 && p.y < map.size

  def fixedPoint(map: Seats)(adjacency: (Point => Option[Boolean]) => (Point, Point) => Boolean, turnover: Int): Int = {
    def iterate(state: Seats): Int = {
      val next = state.zipWithIndex.map(r => r._1.zipWithIndex.map { e =>
        val p = Point(e._2, r._2)
        val occupied = directions.count(d => adjacency(q => state(q.y)(q.x))(p + d, d))
        e._1.map(s => if (s) occupied < turnover else occupied == 0)
      })
      if (next == state) state.flatten.count(_.exists(identity)) else iterate(next)
    }
    iterate(map)
  }

  def scan(map: Seats)(f: Point => Option[Boolean])(p: Point, direction: Point): Boolean =
    inBounds(map, p) && f(p).getOrElse(scan(map)(f)(p + direction, direction))

  def part1 (inp: Seats) = fixedPoint(inp)(f => (p, _) => inBounds(inp, p) && f(p).exists(identity), 4)

  def part2 (inp: Seats) = fixedPoint(inp)(scan(inp) _, 5)

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day11.txt").getLines()
      .map{x =>
        x.toVector.map{
        case '#' => Some(true)
        case 'L' => Some(false)
        case _   => None
        }
      }.toVector

    println(part1(inp))
    println(part2(inp))
  }
}
