object Main {

  def checkValid1 (inp: List[(String, String)]): Boolean = inp.length match {
    case 8 => true
    case 7 => inp.forall(_._1 != "cid")
    case _ => false
  }

  def checkValid2 (inp: List[(String, String)]): Boolean = {
    def checkVHelper (rest: List[(String, String)]): Boolean = {
      rest match {
        case List()    => true
        case (x :: xs) => 
          val p = checkTup(x._1, x._2)
          if (p) checkVHelper(xs) else false
      }
    }

    def checkDig (v: String, min: Int, max: Int, l: Int = 4): Boolean = {
      if (v.matches(s"\\d{$l}")) {
        val x = Integer.parseInt(v)
        x >= min && x <= max 
      } else false
    }

    def checkHgt (v: String): Boolean = {
      if (v.matches("\\d{2}in"))
        checkDig(v.substring(0, 2), 59, 76, 2)
      else if (v.matches("\\d{3}cm"))
        checkDig(v.substring(0, 3), 150, 193, 3)
      else false
    }

    def checkTup (key: String, v: String): Boolean = key match {
      case "byr" => checkDig (v, 1920, 2002) 
      case "iyr" => checkDig (v, 2010, 2020)
      case "eyr" => checkDig (v, 2020, 2030)
      case "hgt" => checkHgt (v)
      case "hcl" => v.matches("#[0-9a-f]{6}")
      case "ecl" => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)
      case "pid" => v.matches("\\d{9}")
      case "cid" => true
      case _     => false
    }

    checkVHelper(inp)
  }

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day4.txt").mkString.split("\n\n").map(_.split("\\s+|\n"))
    val mapped = for (x <- inp; tup = for(y <- x; z = y.split(":")) yield (z(0).trim, z(1).trim)) yield tup.toList
    print(mapped.filter(l => checkValid1(l) && checkValid2(l)).length)
  }
}
