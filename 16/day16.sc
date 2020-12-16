#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 

import fastparse._, SingleLineWhitespace._
import scala.collection.mutable

val startTime = System.currentTimeMillis()

// Parse the input into a single string.
val input = scala.io.Source.fromFile("input.txt").mkString

type Ticket = Seq[Int]
case class Rule(name: String, ranges: Seq[Range]) {
  def matchesValue(value: Int) = ranges.exists{ r => r contains value}
}
case class File(rules: Seq[Rule], yourTicket: Ticket, otherTickets: Seq[Ticket]) {
  val fieldCount = yourTicket.length
  def allRuleRanges = rules.flatMap(_.ranges)
  def isValidValue(value: Int) = allRuleRanges.exists(r => r contains value)
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
  def ticketLine[_: P] = P(num.rep(sep=",") ~ "\n")
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

// Sum the invalid values for all other tickets.
val answerPart1 =
  file.otherTickets
  .flatten
  .filterNot(file.isValidValue)
  .sum

val part1EndTime = System.currentTimeMillis()

val answerPart2 = {
  // Filter out invalid tickets first
  val validTickets = file.otherTickets.filter(_.forall(file.isValidValue))

  // Now look for potential index values for each rules
  val validRules = file.rules.map { rule => 
    val bitset = 
      (0 until file.fieldCount).filter{ i =>
        validTickets.forall{ ticket => rule.matchesValue(ticket(i)) }
      }.to(mutable.BitSet)
    (rule, bitset)
  }

  // Resolve the rule sets by finding a rule with a single index each time.
  def resolveRules(unresolved: Seq[(Rule, mutable.BitSet)], resolved: Map[Rule, Int] = Map()): Map[Rule, Int] = {
    if (unresolved.sizeIs == 0) resolved
    else {
      unresolved.collectFirst { case (r, set) if set.sizeIs == 1 => (r, set.head) } match {
        case Some((rule, int)) =>
          val res = resolved + (rule -> int)
          val unres = unresolved.collect{ case (r, set) if r != rule => (r, set - int)}
          resolveRules(unres, res)
        case None => throw new Exception("Inconceivable")
      }
    }
  }

  // Last part: Filter departure rules and take the product of the ticket values (as a Long)
  resolveRules(validRules)
  .collect {
    case (rule, index) if rule.name.startsWith("departure") =>
      file.yourTicket(index).toLong
  }.product
}
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Parse time, ${parseEndTime - startTime} ms\n" +
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - parseEndTime} ms\n" + 
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

