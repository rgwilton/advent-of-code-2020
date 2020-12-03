#!amm
// scala 2.13.2
import scala.collection.immutable.SortedSet

// Read input, convert to ints, and store in a set.
val input = os.read.lines(os.pwd / "input.txt").map(_.toInt).to(SortedSet)

val candidates = input.iterator.takeWhile(_ < 1010)

// Iterate, finding the first element with a peer in the set that sums to 2020.
val answer = 
  candidates
  .find { x => input.contains(2020 - x)}
  .map { x => x * (2020 - x) }

answer match {
  case Some(x) => println(x)
  case None => println("No match found")
}