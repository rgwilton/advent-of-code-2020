#!amm
// scala 2.13.2

import fastparse._, SingleLineWhitespace._

case class Record(fields: Map[String, String]) {
    def isValid = fields.size == 8 || 
                 (fields.size == 7 && !fields.contains("cid"))
}

def isWsp(c: Char) = (c == ' ' || c == '\n')
def fieldName[_ :P] = P(CharIn("a-z").rep(3).!)
def fieldValue[_ :P] = P(CharsWhile(!isWsp(_)).!)
def field[_: P] = P(fieldName ~/ ":" ~/ fieldValue)
def fSep[_: P] = P(" " | "\n")
def record[_ :P] = P(field.repX(sep = fSep)).map{ fields => Record(fields.toMap) }
def rSep[_: P] = P("\n\n")
def file[_: P] = (record.rep(sep = rSep))

// Parse the input into a strings.
val input = scala.io.Source.fromFile("input.txt").mkString

val Parsed.Success(records, _) = parse(input, file(_))
val answer = records.filter(_.isValid).length

println(answer)
