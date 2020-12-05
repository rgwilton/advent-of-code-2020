#!amm
// scala 2.13.2

// Parse the input into a strings.
val input = scala.io.Source.fromFile("input.txt").getLines


def lineToInt(line: String) = {
  val binStr = line.map { case 'B' | 'R' => '1'; case _ => '0' }
  Integer.parseInt(binStr, 2)
}

// Iterate, finding the first element with a peer in the set that sums to 2020.
val answer = 
  input
  .map(lineToInt)
  .toSeq
  .sorted
  .last

println(answer)
