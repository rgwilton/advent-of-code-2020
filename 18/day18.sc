#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 
import fastparse._, SingleLineWhitespace._

val startTime = System.currentTimeMillis()

// Parse the input
def input = scala.io.Source.fromFile("input.txt").getLines

// Common definitions
def number[_: P] = P( CharsWhileIn("0-9").!.map(_.toLong) )

// Evaluate the expression as it is parsed.
def eval(tree: (Long, Seq[(String, Long)])) = {
  val (base, ops) = tree
  ops.foldLeft(base){ case (left, (op, right)) => op match {
    case "+" => left + right
    case "*" => left * right
  }}
}

object Part1 {
  // Treat operators as having the same precedence.
  def op[_: P] = P(CharIn("+*").!)
  def parens[_: P] = P( "(" ~/ addMul ~ ")" )
  def factor[_: P] = P( number | parens )
  def addMul[_: P]: P[Long] = P( factor ~ (op ~/ factor).rep).map(eval)
  def expr[_: P] = P( addMul ~ End )

  def parseLine(input: String): Long =
    parse(input, expr(_)) match { case Parsed.Success(output, _) => output }
}

object Part2 {
  // Treat + as having a higher precedence than *
  def parens[_: P] = P( "(" ~/ mul ~ ")" )
  def factor[_: P] = P( number | parens )
  def add[_: P]: P[Long] = P( factor ~ ("+".! ~/ factor).rep).map(eval)
  def mul[_: P]: P[Long] = P( add ~ ("*".! ~/ add).rep).map(eval)
  def expr[_: P] = P( mul ~ End )

  def parseLine(input: String): Long =
    parse(input, expr(_)) match { case Parsed.Success(output, _) => output }
}

val answerPart1 = input.map(Part1.parseLine).sum
val part1EndTime = System.currentTimeMillis()

val answerPart2 = input.map(Part2.parseLine).sum
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" + 
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

