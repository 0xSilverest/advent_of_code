object Day7 {

  case class BagRule (parent: String, cost: Int, child: String)

  def findPar (map: Set[BagRule], bag: String = "shiny gold"): Set[String] = 
    map.filter(_.child == bag).flatMap (x => findPar(map, x.parent)) + bag
  
  def findChildCost (map: Set[BagRule], bag: String = "shiny gold"): Int = 
    map.filter(_.parent == bag).foldLeft (1) ((x, y) => x + y.cost * findChildCost(map, y.child))

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day7.txt").getLines()
      .filterNot(_.contains("other"))
      .map(_.split("bags contain"))
      .map(x => (x(0).trim, x(1).replaceAll("bags|bag|\\.", "").split(",").map(_.trim).toList))
      .flatMap {l => 
        val parent = l._1
        val fields = l._2.map(_.split(" ", 2))
        
        fields.map { f =>
          BagRule(parent, f(0).toInt, f(1))
        }
      }.toSet
   
    println(findPar(inp).size - 1)
    println(findChildCost(inp) - 1)
  }
}
