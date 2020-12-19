#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 
import fastparse._, NoWhitespace._

val startTime = System.currentTimeMillis()

// Parse the input
def input = scala.io.Source.fromFile("small_input_part2.txt").getLines.mkString("", "\n", "\n")

trait Rule
case class Literal(c: Char) extends Rule
case class SeqRule(rules: Seq[Int]) extends Rule
case class AltRule(first: Rule, alternative: Rule) extends Rule

object Parser {
  // Treat operators as having the same precedence.
  def number[_: P] = P( CharsWhileIn("0-9").!.map(_.toInt) )
  def seqNumbers[_ :P] = P(number.rep(sep=" ")).map(SeqRule)
  def literal[_: P] = P("\"" ~/ AnyChar.! ~/ "\"").map(ch =>Literal(ch(0)))
  def alternation[_:P] = P(seqNumbers ~ " | " ~/ seqNumbers).map {
    case (firstMatch, secondMatch) => AltRule(firstMatch, secondMatch)
  }
  def ruleLine[_: P] = P(number ~ ": " ~/ (literal | alternation | seqNumbers) ~ "\n").map {
    case (index, rule) => index -> rule
  }
  def rules[_: P] = P(ruleLine.repX).map(_.toMap)
  def testLine[_: P] = P( CharsWhileIn("ab").! ~ "\n" )
  def testLines[_:P] = P(testLine.repX)
  def file[_: P] = P( rules ~ "\n" ~ testLines ~ End )

  def parseLine(input: String) =
    parse(input, file(_)) match { case Parsed.Success(output, _) => output }
}

var (rules, testLines) = Parser.parseLine(input)

// Evaluate the expression as it is parsed.
def check(r: Rule, input: List[Char]): (Boolean, List[Char]) = {
  if (input.isEmpty) (false, input)
  else
    r match {
      case Literal(c) => 
        if (c == input.head) (true, input.tail)
        else (false, input) 
      case AltRule(first, second) =>
        check(first, input) match {
          case x@(true, remaining) => x
          case _ => check(second, input)
        }
      case SeqRule(rs) => 
        var (ok, remaining) = (true, input)
        val iter = rs.iterator
        while (ok && iter.hasNext) {
          val res = check(rules(iter.next), remaining)
          ok = res._1; remaining = res._2
        }
        if (iter.hasNext) ok = false
        (ok, remaining)
    }
}

def checkLine(line: String) = {
  val (ok, remaining) = check(rules(0), line.toList)
  if (ok && remaining.isEmpty) true else false
}

val answerPart1 = {
  testLines.count(checkLine)
}
val part1EndTime = System.currentTimeMillis()

val answerPart2 = {
  // Update rules:
  // 8: 42 | 42 8
  // 11: 42 31 | 42 11 31
  rules += (8 -> AltRule(SeqRule(Seq(42)), SeqRule(Seq(42, 8))))
  rules += (11 -> AltRule(SeqRule(Seq(42, 31)), SeqRule(Seq(42, 11, 31))))

  println(checkLine("babbbbaabbbbbabbbbbbaabaaabaaa"))

  testLines.filter(checkLine).foreach(println)

  testLines.count(checkLine)
}
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

