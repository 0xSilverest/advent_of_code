object Day8 {
  
  case class BootCode (op: String, arg: Int)
  

  def accRecurse (inp: Array[BootCode], i: Int = 0,visitedLines: Set[Int] = Set.empty[Int], acc: Int = 0): (Int, Boolean) = {
    if (visitedLines.contains(i) || i == inp.length) (acc, visitedLines.contains(i)) 
    else { 
      val (arg, inc) = checkLine (inp(i))
      accRecurse(inp, i + inc, visitedLines + i, acc + arg)       
    }
  }

  def checkLine (code: BootCode) = 
    code.op match {
      case "acc"  => (code.arg, 1)
      case "jmp"  => (0, code.arg)
      case "nop"  => (0, 1)
    }


  def thatImperativeFunctionThatWannaBeFunc (inp: Array[BootCode]): Int = accRecurse(inp)._1

  def impWannaBeFuncLul (inp: Array[BootCode]): Int = {
    def checker(i: Int): Int = { 
      if (i == inp.length) -1
      else 
        inp(i).op match {
          case "acc" => checker(i+1)
          case "jmp" => thisCodeIsShit(i, "nop")
          case "nop" => thisCodeIsShit(i, "jmp")
        }
    }

    def thisCodeIsShit (i: Int, inst: String) = {
      val tempInp = inp.clone() 
      tempInp(i) = new BootCode (inst, inp(i).arg)
      val tup = accRecurse(tempInp)
      if(tup._2 == false) tup._1
      else checker(i+1)
    }
    
    checker(0)
  }

  def part1 (inp: Array[BootCode]) = thatImperativeFunctionThatWannaBeFunc(inp)

  def part2 (inp: Array[BootCode]) = impWannaBeFuncLul(inp)

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day8.txt").getLines()
          .map{x => val f = x.split(" ")
                    BootCode(f(0).toString(), f(1).toInt)}.toArray

    println(part1(inp))
    println(part2(inp))
  }
}
