#!amm
// scala 2.13.2

import scala.io.AnsiColor._
import fastparse._, SingleLineWhitespace._

// Read the input into a single string.
val input = scala.io.Source.fromFile("input.txt").mkString

object Record {
  // Regexes for helping to check fields.
  val year = """(\d{4})""".r
  val hairColour = """(#[0-9a-f]{6})""".r
  val pid = """(\d{9})""".r
  val heightCm = """(\d+)cm""".r
  val heightIn = """(\d+)in""".r

  val eyeColours = Set("amb","blu","brn","gry","grn","hzl","oth")
}
case class Record(fields: Map[String, String]) {
  import Record._

  def isValid = isFieldCountOK && allFieldsValid

  def isFieldCountOK = 
    fields.size == 8 || (fields.size == 7 && !fields.contains("cid"))
                
  def isIntValid(intStr: String, min: Int, max: Int) =
    intStr.toIntOption match {
      case Some(i) if i >= min & i <= max => true
      case _ => false
    }

  def isFieldValid(fieldVal: (String, String)) = 
    fieldVal match {
      case ("byr", ystr) if isIntValid(ystr, 1920, 2002) => true
      case ("iyr", ystr) if isIntValid(ystr, 2010, 2020) => true
      case ("eyr", ystr) if isIntValid(ystr, 2020, 2030) => true
      case ("hgt", heightCm(hstr)) if isIntValid(hstr, 150, 193) => true
      case ("hgt", heightIn(hstr)) if isIntValid(hstr, 59, 76) => true
      case ("hcl", hairColour(_)) => true
      case ("ecl", colStr) if eyeColours.contains(colStr) => true
      case ("pid", pid(_)) => true
      case ("cid", _) => true
      case _ => false
    }

  def allFieldsValid = fields.forall(isFieldValid)

  // Add colours to the field output to help with debugging.
  override def toString = {
    def colour(fv: (String, String)) = 
      if (isFieldValid(fv)) s"${GREEN}" else s"${RED}"

    fields.toSeq.sortBy{ case (name, _) => name }
    .map {
      case f @ (name, str) => s"${colour(f)}$name:$str${RESET}"
    }.mkString("","\n","\n")
  }
}

// Use the fastparse library to parse the records.
object Parser {
  def isWsp(c: Char) = (c == ' ' || c == '\n')
  def fieldName[_ :P] = P(CharIn("a-z").rep(3).!)
  def fieldValue[_ :P] = P(CharsWhile(!isWsp(_)).!)
  def field[_: P] = P(fieldName ~/ ":" ~/ fieldValue)
  def fSep[_: P] = P(" " | "\n")
  def record[_ :P] = P(field.repX(sep = fSep)).map {
      fields => Record(fields.toMap)
    }
  def rSep[_: P] = P("\n\n")
  def file[_: P] = (record.rep(sep = rSep))
}

// Parse the file (assume that it succeeds)
val Parsed.Success(records, _) = parse(input, Parser.file(_))

// Debug to print out the records and which fields are valid.
//records.foreach(println(_))

val answer = records.filter(_.isValid).length

println(answer)
