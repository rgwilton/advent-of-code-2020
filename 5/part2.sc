#!amm
// scala 2.13.2

// Parse the input into a strings.
val input = scala.io.Source.fromFile("input.txt").getLines

def code2BinStr(c: String) = c.map { case 'B' | 'R' => '1'; case _ => '0' }
def lineToInt(line: String) = Integer.parseInt(code2BinStr(line), 2)

def seatPairToGap(pair: Seq[Int]) =
  pair match {
    case Seq(a, b) if b == a + 2 => Some(a + 1)
    case _ => None
  }

// Iterate, convert to: binary string then base 2 int, map to sorted sequence,
// take consecutive pairs and find the pair with a gap and return the missing number.
val answer = 
  input
  .map(lineToInt)
  .toSeq
  .sorted
  .sliding(2)
  .flatMap(seatPairToGap)
  .next

println(answer)
