import scala.annotation.tailrec

object Day16 {
  
  case class Tickets(departures: Map[String, Set[Range.Inclusive]], own: Seq[Int], other: List[Seq[Int]]) {
    def invalid: Seq[Int] = 
      other.flatten.filterNot(v => departures.values.flatten.exists(_.contains(v)))

    def filterValid: Tickets =
      copy(other = other.filter(vs => vs.forall(v => departures.values.flatten.exists(_.contains(v)))))

    def matchIndex: Map[String, Int] = {
      val f = filterValid.other
            .map(_.map(v => departures.filter {case (_, r) => r.flatten.contains(v)}.keySet))
            .reduce(_ zip _ map {case (l, r) => l.intersect(r)})
      
      def unique(acc: Seq[Set[String]]): Seq[String] =
        if (acc.forall(_.size == 1)) acc.flatMap(_.headOption)
        else {
          val uniques = acc.filter(_.size == 1).flatten
          val removed = acc.map {
            case u if u.size == 1 => u
            case l => l -- uniques
          }
          unique(removed)
        }

      unique(f).zipWithIndex.toMap 
    }
      
    def mapOwn: Map[String, Int] = 
      matchIndex.view.mapValues(i => own(i)).toMap
  }

  object Tickets {
    val empty: Tickets = Tickets(Map.empty, Seq.empty, Nil)
  }

  def part1(inp: Tickets): Int = 
    inp.invalid.sum

  def part2(inp: Tickets): Long = 
    inp.mapOwn.filter {case (k, _) => k.startsWith("departure")}.values
      .map(_.toLong).product

  def main (args: Array[String]): Unit = {
    val inp = scala.io.Source.fromFile("input/day16.txt").mkString.split("\n\n").toSeq
    val departures = inp(0).split("\n")
      .map(_.split("\\s*:\\s*"))
      .map(x => (x(0), x(1).split("\\s*or\\s*").toList.map(_.split("\\s*-\\s*")).map(x => x(0).toInt to x(1).toInt)))
      .map(x => (x._1, x._2.toSet))

    val own = inp(1).split("\\s*:\\s*").drop(1).flatMap(_.split(",")).map(_.toInt).toSeq
    val other = inp(2).split("\\s*:\\s*")(1).split("\n").map(_.split(",").map(_.toInt).toSeq).toList

    val input = Tickets(departures.toMap, own, other)
    println(part1(input))
    println(part2(input))
  }
}
