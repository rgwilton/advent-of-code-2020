#!amm
// scala 2.13.2

val startTime = System.currentTimeMillis()
case class Cmd(op: Char, value: Int)
// Parse the input into an iterator over input lines, then convert to a sequence of numbers.
val input = scala.io.Source.fromFile("input.txt").getLines
val cmds = input.map { str => Cmd(str.head, str.tail.toInt) }.toSeq

def part1(commands: Iterable[Cmd]): Int = {
  var (x, y) = (0, 0)
  var heading = 90
  for (c <- commands) c.op match {
    case 'N' => y += c.value
    case 'S' => y -= c.value
    case 'E' => x += c.value
    case 'W' => x -= c.value
    case 'L' => heading = (heading + 360 - c.value) % 360
    case 'R' => heading = (heading + c.value) % 360
    case 'F' => heading match {
      case 0 => y += c.value
      case 90 => x += c.value
      case 180 => y -= c.value
      case 270 => x -= c.value
    }
  }

  x.abs + y.abs
}

def part2(commands: Iterable[Cmd]): Int = {
  var (sx, sy) = (0, 0)
  var (wx, wy) = (10, 1)
  for (c <- commands) c.op match {
    case 'N' => wy += c.value
    case 'S' => wy -= c.value
    case 'E' => wx += c.value
    case 'W' => wx -= c.value
    case 'L' => (c.value, wx, wy) match {
      case (90, x, y) => wx = -y; wy = x
      case (180, x, y) => wx = -x; wy = -y
      case (270, x, y) => wx = y; wy = -x
    }
    case 'R' => (c.value, wx, wy) match {
      case (90, x, y) => wx = y; wy = -x
      case (180, x, y) =>  wx = -x; wy = -y
      case (270, x, y) => wx = -y; wy = x      
    }
    case 'F' =>
      sx += wx * c.value
      sy += wy * c.value
  }

  sx.abs + sy.abs
}

// Count the 1's and 3's.
val answerPart1 = part1(cmds)
val part1EndTime = System.currentTimeMillis()

val answerPart2 = part2(cmds)
val part2EndTime = System.currentTimeMillis()


println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" + 
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)
