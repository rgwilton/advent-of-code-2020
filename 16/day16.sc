#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 

import fastparse._, SingleLineWhitespace._
import scala.collection.mutable

val startTime = System.currentTimeMillis()

// Parse the input into a single string.
val input = scala.io.Source.fromFile("input.txt").mkString

case class Ticket(values: Seq[Int])
case class Rule(name: String, ranges: Seq[Range])
case class File(rules: Seq[Rule], yourTicket: Ticket, otherTicket: Seq[Ticket]) {
  def allRuleRanges = rules.flatMap(_.ranges)
  def allOtherValues = otherTicket.flatMap(_.values)
}

object Parser {
  def num[_: P] = P(CharsWhileIn("0-9").!).map(_.toInt)
  def range[_: P] = P(num ~ "-" ~ num).map{ 
    case (low, high) => low to high
  }
  def ruleName[_: P] = P(CharsWhile(_ != ':').!)
  def rule[_: P] = P(ruleName ~ ":" ~ range ~/ "or" ~/ range ~ "\n").map {
    case (name, r1, r2) => Rule(name, Seq(r1, r2))
  }
  def rules[_: P] = P(rule.rep ~ "\n")
  def ticketLine[_: P] = P(num.rep(sep=",") ~ "\n").map(Ticket)
  def yourTicket[_: P] = P("your ticket:\n" ~/ ticketLine ~ "\n")
  def nearbyTickets[_: P] = P("nearby tickets:\n" ~/ ticketLine.rep)
  def file[_: P] = P(rules ~/ yourTicket ~/ nearbyTickets ~ End).map {
    case (rules, yourT, nearbyT) => File(rules, yourT, nearbyT)
  }

  def parseFile(input: String): File =
    parse(input, file(_)) match { 
      case Parsed.Success(output, _) => output
      case Parsed.Failure(stack, idx, extra) => 
      throw new Exception(extra.traced.trace)
    }
}

val file = Parser.parseFile(input)
val parseEndTime = System.currentTimeMillis()

val answerPart1 = {
  def isValidValue(f: File)(value: Int) = f.allRuleRanges.exists(r => r contains value)
  file.allOtherValues.filterNot(isValidValue(file)).sum
}
val part1EndTime = System.currentTimeMillis()

//val answerPart2 = ???
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Parse time, ${parseEndTime - startTime} ms\n" +
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - parseEndTime} ms\n"
//  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

