#!amm
// scala 2.13.2

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val inputSeq = input.map(_.toLong).toSeq

val SetSize = 25

// Check whether there are two numbers in the first 'SetSize' numbers that sum to the target.
def validSum(nums: Seq[Long]): Boolean = {
  val (candidates, Seq(target)) = nums.splitAt(SetSize)
  candidates.exists { c => (c * 2 != target ) && candidates.contains(target - c) }
}

// Split the input into sliding sequences of 26 numbers, filter out the valid ones, and return
// the target number of the first sequence that fails the filter.
val answerPart1 =
  inputSeq
  .sliding(SetSize + 1)
  .filterNot(validSum)
  .nextOption()
  .map(_.last)

println(s"Part 1 answer: ${answerPart1.getOrElse("None")}")

// Part 2
// Find continuous sequence of 2 or more numbers that sum to the total from part 1.
def findSeq(targetTotal: Long): Option[Seq[Long]] = {
  var i = 0
  var j = 1
  var sum = inputSeq(i) + inputSeq(j)
  while (sum != targetTotal && j <= inputSeq.length) {
    if (sum < targetTotal) { j += 1; sum += inputSeq(j) }
    if (sum > targetTotal) { sum -= inputSeq(i); i += 1 }
  }
  sum match {
    case targetTotal => Some(inputSeq.slice(i, j + 1))
    case _ => None
  }
}
val answerPart2 = findSeq(answerPart1.get).map(s => s.min + s.max)

println(s"Part 2 answer: ${answerPart2.getOrElse("None")}")
