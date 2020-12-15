#!amm
// scala 2.13.2

import scala.collection.mutable
val startTime = System.currentTimeMillis()

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = Seq(14,3,1,0,9,5)

def gameIterator(input: Seq[Int]) = {
  // Map from number to the index when it was last seen.
  val map = mutable.Map[Int, List[Int]]()
  var last = 0

  Iterator.from(1).map { i =>
    val next = 
      if (i <= input.length) input(i - 1)
      else map.getOrElse(last, List()) match {
        case (a :: b :: xs) => (a - b)
        case xs => 0
      }
    
    map(next) = i :: (map.getOrElse(next, List()).take(1)) 
    last = next
    next
  }
}

// Count the 1's and 3's.
val answerPart1 = gameIterator(input).drop(2019).next
val part1EndTime = System.currentTimeMillis()

val answerPart2 = gameIterator(input).drop(30000000 - 1).next
val part2EndTime = System.currentTimeMillis()

println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)
