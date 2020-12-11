import scala.collection.mutable
object Day10 {
  def part1 (inp: List[Int]) = {
    val l = 0 :: inp.sorted
    val l2 = l.zip(l.tail)
    l2.filter(x => x._2 - x._1 == 1).length * (l2.filter(x => x._2 - x._1 == 3).length + 1)
  }

  def findTotal (adapters: List[Int], pathMap: Map[Int, Long] = Map(0 -> 1L), lastElem: Int): Long = 
    adapters match {
      case List () => pathMap(lastElem)
      case x :: xs => {
        val s : Long = (1 to 3).map (l => pathMap.get(x - l)).filterNot(_ == Option.empty).map(_.get).sum
        findTotal (xs, pathMap ++ Map(x -> s), x)
      }
    }

  def part2 (inp: List[Int]) = 
    findTotal(inp.sorted, lastElem = inp.head)

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day10.txt").getLines().map(_.trim.toInt).toList
    println (part1(inp))
    println (part2(inp))
  }
}
