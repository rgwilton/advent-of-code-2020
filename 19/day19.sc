#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 
import fastparse._, NoWhitespace._

val startTime = System.currentTimeMillis()

// Parse the input
def input = scala.io.Source.fromFile("input.txt").getLines.mkString("", "\n", "\n")

trait Rule
case class Literal(c: Char) extends Rule
case class SeqRule(rules: Seq[Int]) extends Rule
case class AltRule(alternatives: Seq[Rule]) extends Rule

object Parser {
  // Treat operators as having the same precedence.
  def number[_: P] = P( CharsWhileIn("0-9").!.map(_.toInt) )
  def seqNumbers[_ :P] = P(number.rep(sep=" ")).map(SeqRule)
  def literal[_: P] = P("\"" ~/ AnyChar.! ~/ "\"").map(ch => Literal(ch(0)))
  def alternation[_:P] = P(seqNumbers ~ " | " ~/ seqNumbers).map {
    case (firstMatch, secondMatch) => AltRule(Seq(firstMatch, secondMatch))
  }
  def ruleLine[_: P] = P(number ~ ": " ~/ (literal | alternation | seqNumbers) ~ "\n").map {
    case (index, rule) => index -> rule
  }
  def rules[_: P] = P(ruleLine.repX).map(_.toMap)
  def testLine[_: P] = P( CharsWhileIn("ab").! ~ "\n" )
  def testLines[_:P] = P(testLine.repX)
  def file[_: P] = P( rules ~ "\n" ~ testLines ~ End )

  def parseLine(input: String) =
    parse(input, file(_)) match {
      case Parsed.Success(output, _) => output
    }
}

var (rules, testLines) = Parser.parseLine(input)

// Evaluate the expression as it is parsed.
// Returns the lists of possible remaining characters after the rule has been processed.
def check(r: Rule, input: List[Char]): Seq[List[Char]] = {
  if (input.isEmpty) Seq()
  else
    r match {
      case Literal(c) => 
        if (c == input.head) Seq(input.tail) else Seq()

      case AltRule(alternatives) =>
        alternatives.flatMap(r => check(r, input))
      
      case SeqRule(rs) => 
        rs.foldLeft(Seq(input)) {
          case (Seq(), ruleIndex) => Seq()
          case (xs, ruleIndex) => 
            xs.flatMap(check(rules(ruleIndex), _))
        }
    }
}

def checkLine(line: String) = {
  check(rules(0), line.toList).exists(_.isEmpty)
}

val answerPart1 = {
  testLines.count(checkLine)
}
val part1EndTime = System.currentTimeMillis()

val answerPart2 = {
  // Update rules:
  // 8: 42 | 42 8
  // 11: 42 31 | 42 11 31
  rules += (8 -> AltRule(Seq(SeqRule(Seq(42)), SeqRule(Seq(42, 8)))))
  rules += (11 -> AltRule(Seq(SeqRule(Seq(42, 31)), SeqRule(Seq(42, 11, 31)))))

  testLines.count(checkLine)
}
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

