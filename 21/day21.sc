#!amm
// scala 2.13.2

import $ivy.`com.lihaoyi::fastparse:2.2.2` 
import fastparse._, NoWhitespace._

val startTime = System.currentTimeMillis()

// Parse the input
def input = scala.io.Source.fromFile("input.txt").getLines.mkString("", "\n", "\n")

type Ingredient = String
type Allergen = String
case class Food(ingredients: Seq[Ingredient], allergens: Seq[Allergen])

object Parser {
  // mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
  def word[_: P] = P( CharsWhileIn("a-z").!)
  def allergens[_: P] = P(" (contains " ~/ word.rep(sep=", ") ~ ")")
  def food[_: P] = P (word.repX(sep=" ") ~ allergens ~ "\n").map {
     case (i, a) => Food(i, a)
  }
  def file[_: P] = P( food.rep ~ End )

  def parseLine(input: String) =
    parse(input, file(_)) match {
      case Parsed.Success(output, _) => output
    }
}

val foods = Parser.parseLine(input)

// Common solution

// Map from allergen to Food.
val allergenFoodMap = foods.flatMap(f => f.allergens.map(a => (a -> f))).groupMap(_._1)(_._2)

def commonIngredients(foods: Seq[Food]): Seq[Ingredient] =
  foods.map(_.ingredients).reduce{ (a, b) => a.intersect(b)}

// Map from allergen to all possible ingredients.
var possibleAllergenMap = allergenFoodMap.map{ case (allergen, foods) => allergen -> commonIngredients(foods) }

// Use the same resolver as for an earlier problem.
def resolve(unresolved: Map[Allergen, Seq[Ingredient]],
            resolved: Map[Allergen, Ingredient] = Map()): Map[Allergen, Ingredient] = {
  if (unresolved.sizeIs == 0) resolved
  else {
    unresolved.find{ case (a, ingreds) => ingreds.sizeIs == 1} match {
      case Some((a, Seq(ingred))) =>
        val res = resolved + (a -> ingred)
        val unres = (unresolved - a).mapValues(is => is.filterNot(_ == ingred)).toMap
        resolve(unres, res)
      case None => throw new Exception("Inconceivable")
    }
  }
}

// Map from allergen to food.
val allergenMap = resolve(possibleAllergenMap, Map())

val answerPart1 = {
  val allergenIngredients = allergenMap.values.toSet
  def allIngredients = foods.flatMap(_.ingredients)
  def okIngredients = allIngredients.filterNot(allergenIngredients.contains)

  okIngredients.size
}
val part1EndTime = System.currentTimeMillis()

val answerPart2 = allergenMap.toSeq.sorted.map(_._2).mkString(",")
val part2EndTime = System.currentTimeMillis()
    
println(
  s"Part 1 answer: ${answerPart1}, in ${part1EndTime - startTime} ms\n" +
  s"Part 2 answer: ${answerPart2}, in ${part2EndTime - part1EndTime} ms"
)

