import scala.annotation.tailrec
import scala.io.Source

object day21 extends App {
  val input = Source.fromFile("day21input.txt").getLines().toVector
  val dishRegex = raw"(.*) \(contains (.*)\)".r
  val dishes = input.map {
    case dishRegex(ingredients, allergens) =>
      allergens.split(", ").toSet -> ingredients.split(" ").toSet
  }
  val allergens = dishes.flatMap(_._1).distinct

  @tailrec
  def getMapping(dishes: Vector[(Set[String], Set[String])],
                 untakenAllergens: Set[String] = allergens.toSet,
                 acc: Map[String, String] = Map()): Map[String, String] =
    if (untakenAllergens.isEmpty) acc
    else {
      val (possibleAllergen, Vector(possibleIngredient)) = untakenAllergens
        .map(
          a =>
            a -> dishes
              .filter(_._1.contains(a))
              .map(_._2)
              .reduce(_.intersect(_))
              .toVector
        )
        .filter(_._2.size == 1)
        .head

      val newDishes = dishes
        .map {
          case (allergens, ingredients) =>
            (allergens - possibleAllergen) -> (ingredients - possibleIngredient)
        }
        .filter(_._1.nonEmpty)
      val newAcc = acc + (possibleAllergen -> possibleIngredient)

      getMapping(newDishes, untakenAllergens - possibleAllergen, newAcc)
    }
  val mapping = getMapping(dishes)
  println(dishes.flatMap(_._2).count(a => !mapping.values.toSet.contains(a)))
  // 2 star
  println(mapping.toVector.sortBy(_._1).map(_._2).mkString(","))
}
