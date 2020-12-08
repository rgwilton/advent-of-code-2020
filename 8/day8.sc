#!amm
// scala 2.13.2

// Parse the input into an iterator over input lines.
val input = scala.io.Source.fromFile("input.txt").getLines

object Instr {
  def apply(line: String): Instr = {
    val Array(op, numStr) = line.split(' ')
    Instr(op, numStr.toInt)
  }
}
case class Instr(op: String, arg: Int)

object Program {
  def apply(input: Iterator[String]): Program = 
    Program(input.map(Instr(_)).toIndexedSeq)

}
case class Program(code: IndexedSeq[Instr]) {
  def length = code.length
  def instr(pc : Int) = code(pc)

  // Run the program returning the acc and whether the program terminated.
  def run = {
    var pc = 0
    var acc = 0
    val executed = Array.ofDim[Boolean](code.length)
    def instr = code(pc)
    def terminated = (pc == code.length)
    while (!(terminated || executed(pc))) {
      executed(pc) = true
      instr.op match {
        case "nop" => pc += 1
        case "acc" => acc += instr.arg; pc += 1
        case "jmp" => pc += instr.arg
        case _ => println(s"Invalid instr (${instr.arg}), pc = $pc, acc = $acc")
      }
    }
    (acc, terminated)
  }

  def modifiedAt(i: Int) = {
    val instr = code(i)
    instr.op match {
      case "acc" => None
      case "nop" => Some(Program(code.updated(i, instr.copy(op = "jmp"))))
      case "jmp" => Some(Program(code.updated(i, instr.copy(op = "nop"))))
    }
  }
}

val master = Program(input)

def allVariants: Iterator[Program] = {
  Iterator.range(0, master.length).flatMap { i => master.modifiedAt(i) }
}

// Part 1: Run the first program, and report the acc when the program loops.
val answerPart1 = master.run._1
println(s"Part 1 answer: $answerPart1")

// Part 2: Run the program that terminates and return the acc counter.
val answerPart2 = 
  allVariants.map(_.run).collectFirst { case (acc, terminates) if terminates => acc }
println(s"Part 2 answer: ${answerPart2.getOrElse("None")}")
