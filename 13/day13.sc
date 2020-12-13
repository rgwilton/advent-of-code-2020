#!amm
// scala 2.13.2

val startTime = System.currentTimeMillis()

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines.toSeq
val Seq(timestampStr, busList) = input
val timestamp = timestampStr.toInt
val buses = busList.split(',').filterNot(_ == "x").map(_.toInt).sorted

val nextBus = buses.map { bus =>
  val timeToNext = (bus - (timestamp % bus)) % bus
  (bus, timeToNext)
}
val firstBus = nextBus.sortBy{ case (bus, ttn) => ttn }.head

val answerPart1 = firstBus match { case (bus, ttn) => bus * ttn }
val part1EndTime = System.currentTimeMillis()


// Part 2:
val busOffsets = 
  busList
  .split(',')
  .zipWithIndex
  .collect {
    case (busStr, idx) if busStr != "x" => (busStr.toInt, idx)
  }


// I sort of stumbled on a working solution ...
var x = 0L
var offset = 1L
for ( (bus, idx) <- busOffsets) {
  while ((x % bus) != (idx % bus)) x += offset
  offset *= bus
}

val answerPart2 = offset - x
val part2EndTime = System.currentTimeMillis()


println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" + 
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)
