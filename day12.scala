object Day12 {
  
  type Action = (Char, Int)
  type State = ((Int, Int), Char)
  type ShipWayPoint = (Int, Int)

  def orientation (c: Char) = 
    c match {
      case 'N' => 0
      case 'E' => 1
      case 'S' => 2
      case 'W' => 3
    }

  def orientation (i: Int) = 
    i match {
      case 0 => 'N'
      case 1 => 'E'
      case 2 => 'S'
      case 3 => 'W'
    }

  def move (shipState: State, act: Action) : State = 
    act match {
      case ('N', n) => ((shipState._1._1, shipState._1._2 + n), shipState._2)
      case ('E', n) => ((shipState._1._1 + n, shipState._1._2), shipState._2)
      case ('S', n) => ((shipState._1._1, shipState._1._2 - n), shipState._2)
      case ('W', n) => ((shipState._1._1 - n, shipState._1._2), shipState._2)
      case ('F', n) => move ((shipState._1, shipState._2), (shipState._2, n))
      case ('R', n) => (shipState._1, rotate(shipState._2, n/90))
      case ('L', n) => (shipState._1, rotate(shipState._2, 4 - n/90))
      case other => shipState 
    }

  def moveToPoint (shipState: State, point: ShipWayPoint, act: Action) : (State, ShipWayPoint) =
     act match {
      case ('N', n) => (shipState, (point._1, point._2 + n))
      case ('E', n) => (shipState, (point._1 + n, point._2))
      case ('S', n) => (shipState, (point._1, point._2 - n))
      case ('W', n) => (shipState, (point._1 - n, point._2))
      case ('F', n) => (((shipState._1._1 + point._1 * n, shipState._1._2 + point._2 * n), shipState._2), point)
      case ('R', n) => (shipState, moreRotFuncs(point, n / 90))
      case ('L', n) => (shipState, moreRotFuncs(point, 4 - (n / 90)))
      case other => (shipState, point)
    }
  
  def moreRotFuncs (p: ShipWayPoint, or: Int) : ShipWayPoint = 
    (p, or) match {
      case (_, 0) => p
      case ((x, y), 1) => (y, -x)
      case ((x, y), 2) => (-x, -y)
      case ((x, y), 3) => (-y, x)
      case _ => moreRotFuncs(p, or % 4)
    }

  def rotate(c: Char, i: Int) : Char = orientation((orientation(c) + i) % 4) 

  def manDist (x: (Int, Int)) = math.abs(x._1) + math.abs(x._2)

  def part1 (inp: List[Action]) = manDist(inp.foldLeft(((0, 0), 'E'))(move(_, _))._1)

  def part2 (inp: List[Action]) = manDist(inp.foldLeft((((0,0), 'E'), (10, 1)))((x, y) => moveToPoint(x._1, x._2, y))._1._1)

  def main (args: Array[String]) : Unit = {
    val inp = scala.io.Source.fromFile("input/day12.txt").getLines()
      .map{l => val line = l.trim
                (line(0), line.drop(1).toInt)}
    
    //println(part1(inp.toList))
    println(part2(inp.toList))
  }
}
