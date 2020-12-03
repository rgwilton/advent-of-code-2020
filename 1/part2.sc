#!amm
// scala 2.13.2

// Read input into a sorted indexed sequence.
val input = scala.io.Source.fromFile("input.txt").getLines.map(_.toInt).toIndexedSeq.sorted

// An iterator that starts at a particular index, and stops at a bounded max value.
def boundedIterator(startIndex: Int, maxValue: Int) =
  Iterator.range(startIndex, input.size).takeWhile(x => input(x) <= maxValue)

// Construct an iterator of all possible solutions.
def allInputs =
  for { i <- boundedIterator(0, 2020)
        j <- boundedIterator(i + 1, 2020 - input(i))
        k <- boundedIterator(j + 1, 2020 - input(i) - input(j))
        if input(i) + input(j) + input(k) == 2020 } yield input(i) * input(j) * input(k)

// Take the first solution (if there is one)
val answer = allInputs.nextOption()

answer match {
  case Some(x) => println(x)
  case None => println("No answer")
}

