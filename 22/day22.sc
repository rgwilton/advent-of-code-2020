#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 
import fastparse._, NoWhitespace._
import scala.collection.immutable.Queue
import scala.collection.mutable

val startTime = System.currentTimeMillis()

// Parse the input
def input = scala.io.Source.fromFile("input.txt").getLines.mkString("", "\n", "\n")

object Parser {
  def num[_: P] = P( CharsWhileIn("0-9").!).map(_.toInt)
  def nums[_: P] = P(num.repX(sep = "\n"))
  def player[_: P] = P("Player " ~ ("1" | "2") ~ ":\n" ~/ nums ~ "\n")
  def game[_: P] = P( player ~ "\n" ~ player ~ End )

  def parseGame(input: String) =
    parse(input, game(_)) match {
      case Parsed.Success(output, _) => output
    }
}

def game1(p1: Queue[Int], p2: Queue[Int]): Queue[Int] = {
  if (p1.isEmpty) p2
  else if (p2.isEmpty) p1
  else { 
    val (x, remP1) = p1.dequeue
    val (y, remP2) = p2.dequeue
    if (x > y) game1(remP1 :+ x :+ y, remP2)
    else game1(remP1, remP2 :+ y :+ x)
  }
}

// Implement the algorithm as described.
case class Round(p1: Queue[Int], p2: Queue[Int])
def game2(r: Round, prevRounds: Set[Round] = Set()): (Int, Queue[Int]) = {
  if (r.p1.isEmpty) (2, r.p2)
  else if (r.p2.isEmpty ||
           prevRounds.contains(r)) (1, r.p1)
  else { 
    val (x, remP1) = r.p1.dequeue
    val (y, remP2) = r.p2.dequeue
    val winner = {
      if (remP1.length >= x && remP2.length >= y)
        game2(Round(remP1.take(x), remP2.take(y)))._1
      else if (x > y) 1 else 2
    }
    winner match {
      case 1 => game2(Round(remP1 :+ x :+ y, remP2), prevRounds + r)
      case 2 => game2(Round(remP1, remP2 :+ y :+ x), prevRounds + r)
    }
  }
}

val (p1Input, p2Input) = Parser.parseGame(input)
println("Parsing done")


val answerPart1 = {
  val resultQ = game1(p1Input.to(Queue), p2Input.to(Queue))
  resultQ.toSeq.reverse.zip(Iterator.from(1)).map{pair => pair._1 * pair._2}.sum
}
val part1EndTime = System.currentTimeMillis()

val answerPart2 = {
  val (player, resultQ) = game2(Round(p1Input.to(Queue), p2Input.to(Queue)))
  resultQ.toSeq.reverse.zip(Iterator.from(1)).map{pair => pair._1 * pair._2}.sum
}
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

