#!amm
// scala 2.13.2

type Record = Iterator[String]

// Parse the input into an iterator over input lines.
val input = scala.io.Source.fromFile("input.txt").getLines

// Splits the input into separate records, separated by blank lines.
class RecordIterator(base: Iterator[String]) extends Iterator[Record] {
  def hasNext = base.hasNext
  def next() = base.takeWhile(_ != "")
}

// Convert each line to a set of characters, then take the intersection of all sets
// in a record, then get the size.
def processRecord(r: Record) = r.map(_.toSet).reduce{ (a,b) => a.intersect(b)}.size

val answer = 
  new RecordIterator(input)
  .map(processRecord)
  .sum

println(answer)
