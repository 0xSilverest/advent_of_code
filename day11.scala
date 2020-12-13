object Day11 {

  type Seats = Array[Array[Char]]

  def checkAdjSeat (xs: Seats): Int =
    xs.map(_.count(_ == '#')).sum

  def checkAlignedSeats (xss: Seats): Int = ???

  def nextSeats (xss: Seats): Seats =
    xss.zipWithIndex.map {xs =>
      xs._1.zipWithIndex.map {x =>
        lazy val occAdjSeats = checkAdjSeat (xss.slice(xs._2-1, xs._2+2).map(ys => ys.slice(x._2-1, x._2+2)))
        (x._1, occAdjSeats) match {
          case ('L', 0) => '#'
          case ('#', adj) if adj > 4 => 'L'
          case _ => x._1
        }
      }
    }

  def iCouldJustUseWhile (xss: Seats, last: Seats) : Seats = 
    if(last.map(_.mkString).sameElements(xss.map(_.mkString))) xss else iCouldJustUseWhile(nextSeats(xss), xss)

  def part1 (inp: Seats) = 
    checkAdjSeat(iCouldJustUseWhile(inp.map(_.map(x => if (x == 'L') '#' else x)), inp))

  def part2 (inp: Seats) = ???

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day11.txt").getLines().map(_.toCharArray()).toArray
    println(part1(inp))
  }
}
