#!amm

// Construct a regex match/extractor.
val regex = """(\d+)-(\d+) (\w): (\w+)""".r

// Parse the input (ignoring invalid lines)
val inputLines = scala.io.Source.fromFile("input.txt").getLines.flatMap { 
  case regex(low, high, letStr, passwd) =>
    Some((low.toInt, high.toInt, letStr(0), passwd))
  case _ => None
}

val answer = 
  inputLines.filter {
    case (low, high, letter, passwd) => 
      val count = passwd.count(_ == letter)
      count >= low && count <= high
  }.length

println(answer)
