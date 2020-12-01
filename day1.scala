import scala.io.Source

object day1 extends App {
  val input = Source.fromFile("day1input.txt").getLines().map(_.toInt).toSet

  val a = input.filter(n => input.contains(2020 - n)).head
  println(a * (2020 - a))

  // 2 star
  val twoSums = input.toVector
    .combinations(2)
    .map {
      case Vector(a, b) => (a + b) -> (a, b)
    }
    .toMap

  val c = input.filter(n => twoSums.contains(2020 - n)).head
  val ab = twoSums(2020 - c)
  println(ab._1 * ab._2 * c)
}
