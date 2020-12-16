import scala.annotation.tailrec
import scala.io.Source

object day16 extends App {
  val input = Source.fromFile("day16input.txt").getLines().toVector
  val ruleRegex = raw"(.+): (\d+)-(\d+) or (\d+)-(\d+)".r
  val rules = input
    .takeWhile(_.nonEmpty)
    .map {
      case ruleRegex(name, min1, max1, min2, max2) =>
        name -> (
          (x: Int) =>
            (min1.toInt <= x && x <= max1.toInt) || (min2.toInt <= x && x <= max2.toInt)
        )
    }
    .toMap
  val myTicket =
    input.dropWhile(_ != "your ticket:")(1).split(",").map(_.toInt).toVector
  val nearbyTickets = input
    .dropWhile(_ != "nearby tickets:")
    .tail
    .map(_.split(",").map(_.toInt).toVector)
  println(nearbyTickets.flatten.filter(x => !rules.values.exists(_(x))).sum)

  // 2 star
  val validNearbyTickets =
    nearbyTickets.filter(_.forall(x => rules.values.exists(_(x))))
  val columns = validNearbyTickets.transpose

  @tailrec
  def getRuleMapping(columns: Map[Int, Vector[Int]],
                     rules: Map[String, Int => Boolean],
                     acc: Map[String, Int] = Map()): Map[String, Int] =
    if (columns.isEmpty) acc
    else {
      val unambiguousColumns = columns
        .map {
          case (i, values) =>
            i -> rules.filter(r => values.forall(r._2(_))).keys
        }
        .filter(_._2.size == 1)
        .map { case (i, r) => (i, r.head) }
      val newColumns = columns -- unambiguousColumns.keys
      val newRules = rules -- unambiguousColumns.values
      val newAcc = acc ++ unambiguousColumns.map(_.swap)
      getRuleMapping(newColumns, newRules, newAcc)
    }
  val ruleToIndex =
    getRuleMapping(columns.zipWithIndex.map(_.swap).toMap, rules)
  println(
    ruleToIndex
      .filter(_._1.startsWith("departure"))
      .map(i => BigInt(myTicket(i._2)))
      .product
  )
}
