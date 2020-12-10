#!amm
// scala 2.13.2

import scala.collection.immutable.Queue

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val inputSeq = input.map(_.toInt).toSeq.sorted

val startTime = System.currentTimeMillis()

// Add the end points, calculate the intervals.
val intervals = 
  (0 +: inputSeq :+ (inputSeq.last + 3))
  .sliding(2)
  .map{ case Seq(x, y) => y-x }
  .toSeq

// Count the 1's and 3's.
val answerPart1 = intervals.count(_ == 1) * intervals.count(_ == 3)

val part1EndTime = System.currentTimeMillis()

println(
  s"Part 1 answer: ${answerPart1} in ${part1EndTime - startTime} ms"
)


// val answerPart2 = findSeq2(answerPart1.get).map(s => s.min + s.max)
// val part2EndTime = System.currentTimeMillis()

// println(
//   s"Part 2 answer: ${answerPart2.getOrElse("None")} in ${part2EndTime - part1EndTime} ms"
// )
