#!amm
// scala 2.13.2

// Parse the input into an array of strings.
val array = scala.io.Source.fromFile("input.txt").getLines.toArray
val limit = array(0).length

// Generate an infinite list of x steps bounded to line length.
val x: LazyList[Int] = 0 #:: x.map { x => (x + 3) % limit}
val y = 0 until array.length

// Generate a list of squares that visited, and count those with '#'
val squares = for ((x, y) <- x.zip(y)) yield array(y)(x)
val answer = squares.count(_ == '#')

println(answer)
