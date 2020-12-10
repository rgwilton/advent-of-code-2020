#!amm
// scala 2.13.2

import scala.collection.immutable.Queue
val startTime = System.currentTimeMillis()

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val inputSeq = input.map(_.toInt).toSeq.sorted

def bounded(xs: Seq[Int]) = 0 +: xs :+ (xs.last + 3)
def intervals(xs: Seq[Int]) = xs.sliding(2).map{ case Seq(x, y) => y-x }.toSeq

// Add the end points, calculate the intervals.
val inputIntervals = intervals(bounded(inputSeq))

// Count the 1's and 3's.
val answerPart1 = inputIntervals.count(_ == 1) * inputIntervals.count(_ == 3)

val part1EndTime = System.currentTimeMillis()

// Split the input sequence into sequences that contain intervals smaller than
// 3.
def splitOn3Intervals(intervals: Iterable[Int]): Iterable[Seq[Int]] = {
  var result = List[List[Int]]()
  def c(remaining: List[Int], curList: List[Int]): Unit = {
    remaining match {
      case Nil => 
        if (curList.nonEmpty) result = curList.reverse :: result
      case x :: xs =>
        if (curList.isEmpty || x - curList.head < 3) c(xs, x :: curList)
        else {
          result = curList.reverse :: result
          c(remaining.tail, List(x))
        }
    }
  }
  val iList = intervals.toList
  c(intervals.toList, List())
  result.filter(_.length > 2).reverse
}

// Check whether the sequence is valid (i.e. contains no jumps bigger than 3)
def isValidSeq(s: Seq[Int]) = intervals(s).forall(_ <= 3)

// Calculate all possible combinations that retain the head and tail element.
def allCombinations[T](s: Seq[T]) = {
  val head = s.head
  val last = s.last
  val rem = s.tail.init
  (0 to rem.length).flatMap(rem.combinations(_)).map{head +: _ :+ last}
}

// Calculate the count all valid combinations.
def validCombinationsCount(s: Seq[Int]) = allCombinations(s).count(isValidSeq).toLong

val answerPart2 = 
  splitOn3Intervals(bounded(inputSeq))
  .map(validCombinationsCount)
  .product

val part2EndTime = System.currentTimeMillis()

println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)
