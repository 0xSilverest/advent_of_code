object day5 {

  def sniffer (path: List[Char], minMax: (Int, Int), lowUp: (Char, Char)) : Int = 
    path match {
      case List ()   => minMax._1
      case (d :: ds) => sniffer (ds, possibilities(d, minMax, lowUp), lowUp)
    }

  def possibilities (c: Char, minMax: (Int, Int), lowUp: (Char, Char)) : (Int, Int) = 
    c match {
      case lowUp._1 => ((minMax._1 + minMax._2 + 1) / 2, minMax._2)
      case lowUp._2 => (minMax._1, (minMax._1 + minMax._2 - 1) / 2)
    }

  def findRow (path: String) : Int = sniffer (path.toList, (0, 127), ('B', 'F'))

  def findCol (path: String) : Int = sniffer (path.toList, (0, 7), ('R', 'L'))
  
  def idList (inp: Iterator[String]) : List[Int] = 
    inp.map(x => findRow(x.substring(0, 7)) * 8 + findCol(x.substring(7, 10))).toList

  def main (args: Array[String]) {
    val inp = for(x <- scala.io.Source.fromFile("input/day5.txt").getLines()) yield x
    val ids = idList(inp).sorted
    println(ids.max)
    println(ids.zip(ids.tail).filter(x => x._1 - x._2 == -2).map(_._1 + 1))
  }
}
