object Day13 {

  def checkBusTiming (time: Int, buses: List[Int], busMinTime: (Int, Int)): (Int, Int) =  {
    def calcMinTime (bus: Int, busMinTime : (Int, Int)): (Int, Int) = { 
      val bTime = bus - (time % bus)
      if(bTime > busMinTime._2) busMinTime else (bus, bTime)
    }

    buses match {
      case List () => busMinTime
      case x :: xs => checkBusTiming(time, xs, calcMinTime(x, busMinTime))
    }
  }

  def matchBusTime (time: Long, buses: List[(Long, Long)], step: Long): Long = {
    def recursiveTimeSync (initTime: Long, indexedBus: (Long, Long)): Long = 
      if ((initTime + indexedBus._1) % indexedBus._2 == 0L){
        initTime
      } else recursiveTimeSync(initTime + step, indexedBus)

    buses match {
      case List () => time
      case x :: xs => matchBusTime(recursiveTimeSync(time, x), xs, step * x._2)
    }
  }

  def part1 (inp: (Int, List[String])): Int = {
    val out = checkBusTiming(inp._1, inp._2.map(_.trim).filterNot(_=="x").map(_.toInt), (0, Int.MaxValue))
    out._1 * out._2
  }

  def part2 (inp: (Int, List[String])): Long = {
    val parseInp = inp._2.zipWithIndex.map(x => (x._1.trim, x._2)).filterNot(x => x._1 == "x").map(x => (x._2.toLong, x._1.toLong))
    matchBusTime(0, parseInp.drop(1), parseInp.head._2)
  }

  def main (args: Array[String]) : Unit = {
    val file = scala.io.Source.fromFile("input/day13.txt").getLines().toArray
    val inp = (file(0).toInt, file(1).split(",").toList)
    println(part1(inp))
    println(part2(inp))
  }
}
