import scala.annotation.tailrec

object Day17 {
  trait Point {
    def neighbors: List[Point]
  }
  
  def llRange(lo: Int, hi: Int): LazyList[Int] =
    if (lo >= hi) LazyList.empty
    else LazyList.cons(lo, llRange(lo + 1, hi))

  case class Point3d (x: Int, y: Int, z: Int) extends Point {
    def neighbors: List[Point3d] = 
      (x-1 to x+1).flatMap {dx =>
        (y-1 to y+1).flatMap {dy => 
          (z-1 to z+1).map {dz =>
            Point3d(dx, dy, dz)
          }.filterNot(this == _)
        }
      }.toList

      def == (that: Point3d) = 
        that.x == this.x && 
        that.y == this.y &&
        that.z == this.z
 
  }

  case class Point4d (x: Int, y: Int, z: Int, w: Int) extends Point {
    def neighbors: List[Point4d] = 
      (x - 1 to x + 1).flatMap {dx =>
        (y - 1 to y + 1).flatMap {dy => 
          (z - 1 to z + 1).flatMap {dz =>
            (w - 1 to w + 1).map {dw =>
              Point4d(dx, dy, dz, dw)
            }.filterNot(this == _)
          }
        }
      }.toList

    def == (that: Point4d) = 
      that.x == this.x && 
      that.y == this.y &&
      that.z == this.z &&
      that.w == this.w
  }

  def nextCycle (grid: Map[Point, Boolean]): Map[Point, Boolean] =   
    grid.map { p =>  
      lazy val aliveCells = p._1.neighbors.count(grid.getOrElse(_, false) == true)
        (p._2, aliveCells) match {
          case (false, 3) => p._1 -> true
          case (true, x) if (x == 2 || x == 3) => p._1 -> true
          case _ => p._1 -> false
        }
      }.toMap 

  @tailrec
  def runPoint(rounds: Int, pointFunction: (Int, Int) => Point, grid: Map[Point, Boolean]): Map[Point, Boolean] = {
    if (rounds == 0) grid
    else {
      runPoint(rounds - 1, pointFunction, nextCycle(grid ++ grid.flatMap{p => p._1.neighbors.map(n => if (!grid.contains(n)) (n -> false) else p)}.toMap))
    }
  }

  def parseInp (inp: Seq[String], pointFunction: (Int, Int) => Point): Map[Point, Boolean] = 
    inp.zipWithIndex.flatMap (x => x._1.zipWithIndex.map ( y => pointFunction(x._2, y._2) -> (y._1 == '#'))).toMap

  def part1 (inp: Seq[String]) = 
    runPoint(6, ((x,y) => Point3d(x, y, 0)), parseInp(inp, ((x,y) => Point3d(x, y, 0)))).count(_._2 == true)

  def part2 (inp: Seq[String]) =
    runPoint(6, ((x,y) => Point4d(x, y, 0, 0)), parseInp(inp, ((x, y) => Point4d(x, y, 0, 0)))).count(_._2 == true)

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day17.txt").getLines().toSeq
    println(part1(inp))
    println(part2(inp))
  }
}
