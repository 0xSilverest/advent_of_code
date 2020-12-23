object Day21 { 
  val Pattern = """^(.+) \(contains (.+)\)$""".r

  def buildMap(lines: List[(List[String], List[String])], allergenMap: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] = lines match {
    case Nil => allergenMap
    case (allergens, ingredients) :: tail => {
      val ingredientSet = ingredients.toSet
      val newMap = Map(allergens zip allergens.map{
        allergen => if (allergenMap contains allergen) allergenMap(allergen) & ingredientSet else ingredientSet
      }: _*)
      buildMap(tail, allergenMap ++ newMap)
    }
  }

  def assign(unassignedAllergens: Map[String, Set[String]], assignedAllergens: Map[String, String] = Map.empty): Map[String, String] =
    if (unassignedAllergens.isEmpty)
      assignedAllergens
    else {
      val (allergen, ingredient) = unassignedAllergens.find(_._2.size == 1).get
      val newUnassignedAllergens = unassignedAllergens.map{
        case (k, v) => (k, v -- ingredient)
      } - allergen
      val newAssignedAllergens = assignedAllergens + (allergen -> ingredient.head)
      assign(newUnassignedAllergens, newAssignedAllergens)
    }

  def part1 (assignedAllergens: Map[String, String], ings: List[String]): Int = 
    ings.filterNot(assignedAllergens.values.toSet.contains(_)).size 
  
  def part2 (assignedAllergens: Map[String, String]): String =
    assignedAllergens.keys.toList.sorted.map(assignedAllergens(_)).mkString(",")

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day21.txt").getLines()
      .map {_ match {
        case Pattern(lInp, rInp) => {
          val ings = lInp.split(' ').map(_.trim).toList 
          val alls = rInp.split(',').map(_.trim).toList
          (alls, ings)
          }
        }
      }.toList
  
    val ingredients = input.map (_._2).reduce(_ ++ _)
    val allergens = buildMap(input) 
    val assignedAllergens = assign(allergens)
        
    println(part1(assignedAllergens, ingredients)) 
    println(part2(assignedAllergens))
  }
}
