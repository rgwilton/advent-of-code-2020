#!amm
// scala 2.13.2

type Rule = (WaitArea, Int, Int) => Char

case class WaitArea(grid: IndexedSeq[IndexedSeq[Char]], rule: Rule) {
  val width = grid(0).length
  val height = grid.length

  override def toString = grid.map(_.mkString("")).mkString("","\n", "\n")
  
  def isOccupied(c: Char) = c == '#'
 
  def isOccupiedAt(x :Int, y: Int) =
    if (x >= 0 && x < width && y >= 0 && y < height) isOccupied(grid(y)(x)) else false

  def countAdjacent(x: Int, y: Int) = {
    def r(a: Int, b: Int) = if (isOccupiedAt(x + a, y + b)) 1 else 0
    r(-1, -1) + r(0, -1) + r(1, -1) + 
    r(-1, 0)             + r(1, 0) +
    r(-1, 1) + r(0, 1) + r(1, 1)
  }

  def next: WaitArea = {
    val nextGrid = 
      for (y <- 0 until height) yield {
        for (x <- 0 until width) yield { rule(this, x, y) }
      }
    WaitArea(nextGrid, rule)
  }

  def iterations = {
    def iterate(cur: WaitArea): LazyList[WaitArea] = cur #:: iterate(cur.next)
    iterate(this)
  }

  def countOccupied: Int = {
    var sum = 0
      for (y <- 0 until height) {
        for (x <- 0 until width) { 
          if (grid(y)(x) == '#') sum += 1
      }
    }
    sum
  }
}

val startTime = System.currentTimeMillis()

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val inputSeq = input.toIndexedSeq.map(_.toIndexedSeq)

def rule(w: WaitArea, x: Int, y: Int): Char = {
  val c = w.grid(y)(x)
  val adjCount = w.countAdjacent(x, y)
  c match {
    case 'L' if adjCount == 0 => '#'
    case '#' if adjCount >= 4 => 'L'
    case x => x
  }
}

def fixed(last: WaitArea, was: Iterable[WaitArea]): WaitArea = {
  if (was.head == last) last else fixed(was.head, was.tail)
} 

val start = WaitArea(inputSeq, rule)
val iterations = start.iterations

val result = fixed(iterations.head, iterations.tail)



val part1EndTime = System.currentTimeMillis()

println(result)
val answerPart1 = result.countOccupied

println(
  s"Part 1 answer: ${answerPart1} in ${part1EndTime - startTime} ms"
)


// val answerPart2 = findSeq2(answerPart1.get).map(s => s.min + s.max)
// val part2EndTime = System.currentTimeMillis()

// println(
//   s"Part 2 answer: ${answerPart2.getOrElse("None")} in ${part2EndTime - part1EndTime} ms"
// )
