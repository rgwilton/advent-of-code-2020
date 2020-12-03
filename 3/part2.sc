#!amm
// scala 2.13.2

// Parse the input into an array of strings.
val array = scala.io.Source.fromFile("input.txt").getLines.toArray
val limit = array(0).length

// List of input different input steps.
val steps = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

// Generate a path of index pairs of visited squares based on given input step size.
def path(xStep: Int, yStep: Int) = {
  def xSteps(cur: Int): LazyList[Int] = cur #:: xSteps((cur + xStep) % limit)
  def ySteps = 0 until array.length by yStep
  xSteps(0) zip ySteps
}

// Convert a path to the characters at that location and count the #'s.
def squares(step: (Int, Int)) = 
  (for ((x, y) <- path(step._1, step._2)) yield array(y)(x))
  .count(_ == '#')

// Run the prog for each pair of input steps and multiple the results together.
val answer = steps.map(squares).reduce(_ * _)

println(answer)
