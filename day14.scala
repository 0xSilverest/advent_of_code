import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Day14 {
  case class Input (mask: String, memSp: Map[Long, Long])
    
  val zero = BigInt(0)
  
  def bin2long (bits: BigInt): BigInt =  {
    bits match {
      case `zero` => 0
      case _      => 2 * bin2long (bits / 10) + bits % 10      
    }
  }

  @tailrec
  def balanceBitString (bitList: List[Char]) : List[Char] = {
    if (bitList.length == 36) bitList
    else balanceBitString('0' :: bitList)
  }

  def generateCombs (bits: String): List[String] =
    if(!bits.contains("X")) List(bits)
    else List(bits.replaceFirst("X", "0"), bits.replaceFirst("X", "1")).map(generateCombs(_)).flatten

  def modInput (applyMask: (List[Char], List[Char]) => List[Char]) (inp: Input) : Map[Long, String] = {    

    inp.memSp.map{x => 
      val l = applyMask(inp.mask.toList, balanceBitString(x._2.toBinaryString.toList))
          .dropWhile(y => y=='0').mkString
      (x._1, l)}
  }

  def modInput2 (applyMask: (List[Char], List[Char]) => List[Char]) (inp: Input) : Map[String, Long] = {  
    
    inp.memSp.map{x => 
      val y = applyMask(inp.mask.toList, balanceBitString(x._1.toBinaryString.toList))
      (y, x._2)
    } .map(x => (generateCombs(x._1.mkString), x._2))
      .map(x => x._1.map(y => (y, x._2))).flatten.toMap
  }

  @tailrec
  def parseFileToInp (inp: List[(String, String)], acc: List[Input] = List()): List[Input] = { 
    def findMemSpaces (data: List[(String, String)]): ListMap[Long, Long] = 
      data match {
        case List () => ListMap ()
        case x :: xs => 
          if (x._1.matches("mem\\[\\d+\\]"))
            ListMap (x._1.replaceAll("mem\\[|\\]", "").toLong -> x._2.toLong) ++ findMemSpaces(xs)
          else 
            ListMap ()
      }

    inp match {
      case List () => acc
      case x :: xs => 
        if (x._1 == "mask") parseFileToInp(xs, Input (x._2, findMemSpaces(xs)) :: acc)
        else parseFileToInp(xs, acc)
    }
  }

  def part1 (inp: List[Input]) = {
    def applyMask (mask: List[Char], bits: List[Char]): List[Char] = 
      (mask, bits) match {
        case (List (), List ())   => List ()
        case ('X' :: ms, b :: bs) => b :: applyMask (ms, bs)
        case (m :: ms, _ :: bs)   => m :: applyMask (ms, bs)
      }

    def modWithMask = modInput(applyMask) _
    inp.foldLeft(Map.empty[Long, String])(_ ++ modWithMask(_)).map(x => bin2long(BigInt(x._2))).sum 
  }

  def part2 (inp: List[Input]) = { 
    def applyMask (mask: List[Char], bits: List[Char]): List[Char] = 
      (mask, bits) match {
        case (List (), List())    => List()
        case ('0' :: ms, b :: bs) => b :: applyMask (ms, bs)
        case (m :: ms, _ :: bs)   => m :: applyMask (ms, bs)
      }
    
    def modWithMask = modInput2(applyMask) _
    inp.foldLeft(Map.empty[String, Long])(_ ++ modWithMask(_)).map(x => x._2).sum
  }

  def main (args: Array[String]): Unit = {
    val inp = parseFileToInp(scala.io.Source.fromFile("input/day14.txt").getLines().map(_.split("="))
                .map(x => (x(0).trim, x(1).trim)).toList).reverse

    //inp.foreach(println)
    println(part1(inp))
    println(part2(inp))
  }
}
