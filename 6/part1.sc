#!amm
// scala 2.13.2

// Parse the input into an iterator over input lines.
val input = scala.io.Source.fromFile("input.txt").getLines

type Record = Iterator[String]

// Splits the input into separate records, separated by blank lines.
// Not tail recursive!
def records(base: Iterator[String]): Iterator[Record] =
  if (base.nonEmpty) Iterator(base.takeWhile(_ != "")) ++ records(base)
  else Iterator()

// combined all members of the group into a single string, then convert to a set
// of characters, before taking the size of the set.
def processRecord(r: Record) = r.mkString("").toSet.size

val answer = 
  records(input)
  .map(processRecord)
  .sum

println(answer)
