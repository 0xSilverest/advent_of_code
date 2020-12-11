object Day9 {

  def checkSum (l: List[Long], target: Long): Boolean = 
    l match {
      case List() => false
      case (x :: Nil) => false
      case (x :: xs)  => if (xs.find(_ + x == target).isDefined) true else checkSum (xs, target) 
    }

  def checkPreamble (xs: Array[Long], preamble: Int = 25) : Option[Long] = 
    if (xs.length > preamble) { 
      if (checkSum(xs.take(preamble).toList, xs(preamble))) 
        checkPreamble(xs.drop(1))
      else Option(xs(preamble))
    } else Option.empty[Long]
       
  def sumEveryCombo (xs: List[Long], target: Long) = 
    xs.indices.map(s => xs.indices.drop(s+1).reverse.map(e => xs.slice(s, e)).find(_.sum == target)).filter(_!=None)(0)

  def part1 (inp: Array[Long]) = checkPreamble(inp).get

  def part2 (inp: Array[Long]) = {
    val target = part1(inp)
    val ret = sumEveryCombo( inp.toList.takeWhile(_ != target), target).get
    ret.min + ret.max
  }

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day9.txt").getLines().map(_.trim.toLong).toArray
    println(part1(inp))
    println(part2(inp))
  }
}
