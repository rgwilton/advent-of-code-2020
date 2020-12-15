#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 

import fastparse._, NoWhitespace._
import scala.collection.mutable

val startTime = System.currentTimeMillis()

// Parse the input into a strings.
val input = scala.io.Source.fromFile("input.txt").getLines

trait Instruction
case class Mem(address: Long, value: Long) extends Instruction
case class Mask(mask: String) extends Instruction {
  val xBits = java.lang.Long.parseLong(mask.map { case 'X' => '1'; case _ => '0' }, 2)
  val oneBits = java.lang.Long.parseLong(mask.map { case '1' => '1'; case _ => '0' }, 2)
  val floatingBits = mask.reverse.zipWithIndex.collect{ case (char, idx) if char == 'X' => idx }.toList
}

object Parser {
  // Examples of lines to parse:
  // mask = 0111X10100100X1111X10010X000X1000001
  // mem[50907] = 468673978
  def num[_: P] = P(CharsWhileIn("0-9").!).map(_.toLong)
  def mask[_: P] = P("mask = " ~/ CharsWhileIn("01X").!).map(Mask)
  def mem[_: P] = P("mem[" ~/ num ~/ "] = " ~/ num).map{ case (a, v) => Mem(a, v)}
  def line[_: P] = P(mask | mem)

  def parseLine(input: String): Instruction =
    parse(input, line(_)) match { case Parsed.Success(output, _) => output }
}

val instructions = input.map(Parser.parseLine).toSeq
val parseEndTime = System.currentTimeMillis()

type WriteMemFn = (mutable.Map[Long, Long], Mask, Long, Long) => Unit
def processInstructions(instructions: Seq[Instruction])(writeMem: WriteMemFn) = {
  val memory = mutable.Map[Long, Long]()
  var curMask = Mask("0")

  for {instr <- instructions} {
        instr match {
          case m:Mask => curMask = m
          case Mem(addr, value) => writeMem(memory, curMask, addr, value)
        }
      }
  memory.values.sum
}
val processMemInstructions = processInstructions(instructions) _

// Addresses are correct, fix values.
val answerPart1 = processMemInstructions{ (memory, mask, addr, value) =>
  memory(addr) = (value & mask.xBits) | mask.oneBits
}
val part1EndTime = System.currentTimeMillis()

// Values are correct, fix addresses.
val answerPart2 = processMemInstructions{ (memory, mask, addr, value) =>
  def write(addr: Long, floatingBits: List[Int]): Unit = {
    floatingBits match {
      case Nil => memory(addr) = value
      case x::xs => 
        write(addr, xs)
        write(addr | (1L << x), xs)
    }
  }

  val maskedAddr =  (addr & ~mask.xBits) | mask.oneBits
  write(maskedAddr, mask.floatingBits)
}
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Parse time, ${parseEndTime - startTime} ms\n" +
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - parseEndTime} ms\n" + 
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

