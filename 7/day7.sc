#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 

import fastparse._, NoWhitespace._
import scala.collection.mutable

type BagColour = String

// Parse the input into a strings.
val input = scala.io.Source.fromFile("input.txt").getLines

// Examples of lines to parse:
//   vibrant olive bags contain no other bags.
//   light lime bags contain 3 dark beige bags, 4 clear fuchsia bags, 1 clear beige bag, 3 dotted crimson bags.
def word[_: P] = P(CharsWhileIn("a-z"))
def descriptiveColour[_: P] = (word ~/ " " ~/ word).! 
def bags[_ :P] = P(descriptiveColour ~/ " bag" ~/ "s".?)
def noBags[_ :P] = P("no other bags").!.map { _ => Map[BagColour, Int]() }
def num[_: P] = P(CharsWhileIn("0-9").!).map(_.toInt)
def numBags[_ :P] = P(num ~/ " " ~/ bags).map { case (no, col) => (col, no) }
def listBags[_: P] = P(numBags.rep(sep = ", ").map(_.toMap))
def line[_: P] = P(bags ~/ " contain " ~/ (noBags | listBags) ~/ ".")

def parseLine(input: String): (BagColour, Map[BagColour, Int]) =
  parse(input, line(_)) match { case Parsed.Success(output, _) => output }

// The rawMap represents the input, i.e. map colour to what it contains.
val rawMap: Map[BagColour, Map[BagColour, Int]] = input.map(parseLine).toMap

// The resolveMap maps each colour to a map from colour to total recursive count.
// Recursive, but not tail recursive, hence input length is limited to stack
// depth (possible could use a worker queue instead)
val resolvedMap = {
  val workingMap = mutable.Map[BagColour, Map[BagColour, Int]]()

  // Find the colour in the rawMap, recursively resolve the contains colours,
  // then aggregate the results together into a single map, totalling the counts (using groupMapReduce)
  def resolveRaw(colour: BagColour) =
    rawMap(colour).toSeq.flatMap{
      case (bagColour, count) => 
        Iterable(bagColour -> count) ++
        resolve(bagColour).map{ case (c, i) => (c, i * count)}
    }.groupMapReduce(_._1)(_._2)(_ + _)

  def resolve(colour: BagColour): Map[BagColour, Int] = 
    workingMap.getOrElseUpdate(colour, resolveRaw(colour))

  // Resolve all colours from the input keys.
  rawMap.keys.foreach(resolve(_))
  workingMap.toMap
}

// Part 1: Check how many bags contain "shiny gold" bags.
val answerPart1 = resolvedMap.values.count(_.contains("shiny gold"))
println(s"Part 1 answer: $answerPart1")

// Part 2: Return how many bags a "shiny gold" contains.
val answerPart2 = resolvedMap("shiny gold").values.sum
println(s"Part 2 answer: $answerPart2")
