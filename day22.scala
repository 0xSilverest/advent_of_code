object Day22 {
  
  def play (deck: (List[Int], List[Int])): List[Int] = 
    deck match {
      case (List(), _) => deck._2
      case (_, List()) => deck._1
      case (c1 :: d1, c2 :: d2) => {
        if (c1 == c2)
          play (d1 ::: List(c1), d2 ::: List(c2))
        else if (c1 > c2)
          play (d1 ::: List(c1, c2), d2)
        else 
          play (d1, d2 ::: List(c2, c1))
      }
    }

  def p1Win (d: (List[Int], List[Int]), acc: List[List[Int]]): (Boolean, List[Int]) =
    playRecurse ((d._1.tail ::: List(d._1.head, d._2.head), d._2.tail), d._1 :: d._2 :: acc)

  def p2Win (d: (List[Int], List[Int]), acc: List[List[Int]]): (Boolean, List[Int]) = 
    playRecurse ((d._1.tail, d._2.tail ::: List(d._2.head, d._1.head)), d._1 :: d._2 :: acc)

  def playRecurse (deck: (List[Int], List[Int]), acc: List[List[Int]] = List()): (Boolean, List[Int]) =
    deck match {
      case (List(), _) => (false, deck._2)
      case (_, List()) => (true, deck._1)
      case (c1 :: d1, c2 :: d2) => {
        if (acc.contains(deck._1) || acc.contains(deck._2))
          (true, deck._1) 
        else if (c1 <= d1.length && c2 <= d2.length) {
          if (playRecurse((d1.take(c1), d2.take(c2)))._1)
           p1Win (deck, acc)
          else  p2Win (deck, acc)
        } else if (c1 == c2)
          playRecurse ((d1 ::: List(c1), d2 ::: List(c2)), acc)
        else if (c1 > c2)
          p1Win (deck, acc)
        else 
          p2Win (deck, acc)
      }
    }

  def part1 (deck: (List[Int], List[Int])): Int =
    play(deck).reverse.zip(1 to (deck._1.length + deck._2.length)).map(x => x._1 * x._2).sum

  def part2 (deck: (List[Int], List[Int])): Int =
    playRecurse(deck)._2.reverse.zip(1 to (deck._1.length + deck._2.length)).map(x => x._1 * x._2).sum

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day22.txt").mkString.split("\n\n")
                    .map (x => x.split("\n").drop(1).map(_.toInt).toList)
                    
    println(part1((inp.head, inp.last)))
    println(part2((inp.head, inp.last)))
  }
}
