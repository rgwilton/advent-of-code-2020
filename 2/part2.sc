#!amm
// scala 2.13.2

// Construct a regex match/extractor.
val regex = """(\d+)-(\d+) (\w): (\w+)""".r

// Parse the input (ignoring invalid lines) into a 4 tuple.
val input = scala.io.Source.fromFile("input.txt").getLines.flatMap { 
  case regex(first, sec, letterStr, passwd) => 
    Some((first.toInt, sec.toInt, letterStr(0), passwd))
  case _ => None
}

// Filter the lines to those that match the predicate.
val answer = 
  input.filter {
    case (first, second, letter, passwd) => 
      (passwd(first - 1) == letter) != (passwd(second - 1) == letter)
  }.length

println(answer)
