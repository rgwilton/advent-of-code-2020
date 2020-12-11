#!amm
// scala 2.13.2

type Grid = IndexedSeq[IndexedSeq[Char]]

object WaitArea {
  final val Axis = 
     Seq((-1, -1), (0, -1), (1, -1),
         (-1, 0),           (1, 0),
         (-1, 1),  (0, 1),  (1, 1))
}

abstract class WaitArea(startingGrid: Grid) extends Iterator[WaitArea] {
  protected var grid = startingGrid
  val width = grid(0).length
  val height = grid.length
  var nextGrid = calcNextGrid
  def hasNext = (grid != nextGrid) 

  def rule(x: Int, y: Int): Char

  override def toString = grid.map(_.mkString("")).mkString("","\n", "\n")
  
  def isOccupied(c: Char) = c == '#'
 
  def isOccupiedAt(x :Int, y: Int) =
    if (x >= 0 && x < width && y >= 0 && y < height) isOccupied(grid(y)(x)) else false

  def countOccupied: Int = {
    var sum = 0
      for (y <- 0 until height) {
        for (x <- 0 until width) { 
          if (grid(y)(x) == '#') sum += 1
      }
    }
    sum
  }

  def calcNextGrid: Grid =
    for (y <- 0 until height) yield {
      for (x <- 0 until width) yield { rule(x, y) }
    }
  

  def next: WaitArea = {
    grid = nextGrid
    nextGrid = calcNextGrid
    this
  }
}


class WaitAreaPart1(startingGrid: Grid) extends WaitArea(startingGrid) {
  def countAdjacent(x: Int, y: Int) = {
    var sum = 0
    WaitArea.Axis.foreach {
      case (offsetX, offsetY) => if (isOccupiedAt(x + offsetX, y + offsetY)) sum += 1
    }
    sum
  }

  def rule(x: Int, y: Int): Char = {
    val c = grid(y)(x)
    val adjCount = countAdjacent(x, y)
    c match {
      case 'L' if adjCount == 0 => '#'
      case '#' if adjCount >= 4 => 'L'
      case x => x
    }
  }
}


class WaitAreaPart2(startingGrid: Grid) extends WaitArea(startingGrid) {
  def countPath(x: Int, y: Int) = {
    var sum = 0
    WaitArea.Axis.foreach {
      case (offsetX, offsetY) =>
        def checkPath(p: Int, q: Int): Boolean = 
          if (p < 0 || p >= width || q < 0 || q >= height) false
          else
            grid(q)(p) match {
              case '#' => true
              case 'L' => false
              case _ => checkPath(p + offsetX, q + offsetY)
            }
        if (checkPath(x + offsetX, y + offsetY)) sum += 1      
    }
    sum
  }

  def rule(x: Int, y: Int): Char = {
    val c = grid(y)(x)
    val adjCount = countPath(x, y)
    c match {
      case 'L' if adjCount == 0 => '#'
      case '#' if adjCount >= 5 => 'L'
      case x => x
    }
  }
}


val startTime = System.currentTimeMillis()

// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val inputSeq = input.toIndexedSeq.map(_.toIndexedSeq)

val answerPart1 = new WaitAreaPart1(inputSeq).toSeq.last.countOccupied
val part1EndTime = System.currentTimeMillis()

println(
  s"Part 1 answer: ${answerPart1} in ${part1EndTime - startTime} ms"
)

val answerPart2 = new WaitAreaPart2(inputSeq).toSeq.last.countOccupied
val part2EndTime = System.currentTimeMillis()

println(
  s"Part 2 answer: ${answerPart2} in ${part2EndTime - part1EndTime} ms"
)

