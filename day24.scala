object Day24 {
    case class Point3d (x: Int, y: Int, z: Int) {
      lazy val dirs = Map(
        "e" -> Point3d(1, -1, 0),
        "w" -> Point3d(-1, 1, 0),
        "ne" -> Point3d(1, 0, -1),
        "nw" -> Point3d(0, 1, -1),
        "se" -> Point3d(0, -1, 1),
        "sw" -> Point3d(-1, 0, 1),
      )

      lazy val neighs =
        dirs.map (this + _._2)

      def neighbors(dir: String): Point3d =
        dirs(dir) + this

      def + (that: Point3d): Point3d = 
        Point3d(x + that.x, y + that.y, z + that.z)      
  }
  
  def psToEval(last: Set[Point3d]): Set[Point3d] = 
    last ++ (last.flatMap (p => p.neighs))
  
  def next(last: Set[Point3d]): Set[Point3d] = 
    psToEval(last)
      .filter{t => 
        (last.contains(t), t.neighs.count (last.contains(_))) match {
          case (true, adjHex) if (adjHex == 0 || adjHex > 2) => false
          case (false, 2) => true
          case _ => last.contains(t)
        }
      }
  

  def decTiles(inp: Vector[String]): Set[Point3d] = 
    inp.map(toPoint3d(_))
      .groupBy(x => x)
      .filter(_._2.size % 2 == 1)
      .keySet

  def toPoint3d(str: String) : Point3d = 
    "([ns]?[ew])".r
      .findAllIn(str).map(_.mkString)
      .foldLeft(Point3d(0, 0, 0)) ((x, y) => x.neighbors(y)) 

  def againCouldUseWhile(i: Int, acc: Set[Point3d]): Set[Point3d] = 
    if (i == 0) acc
    else againCouldUseWhile(i - 1, next(acc))
  
  def part1(inp: Vector[String]): Int =
    decTiles(inp).size

  def part2(inp: Vector[String]): Int =
    againCouldUseWhile(100, decTiles(inp)).size

  def main(args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day24.txt").getLines().toVector
    println(part1(inp))
    println(part2(inp))
  }
}
