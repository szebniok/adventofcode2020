import scala.io.Source

object day10 extends App {
  val input = Source.fromFile("day10input.txt").getLines().map(_.toInt).toVector
  val diffs =
    (0 +: input.sorted :+ input.max + 3)
      .sliding(2)
      .map { case Vector(a, b) => b - a }
      .toVector
      .groupMapReduce(id => id)(_ => 1)(_ + _)
  println(diffs(1) * diffs(3))

  // 2 star
  println(input.sorted.foldLeft(Map[Int, Long](0 -> 1)) {
    case (acc, adapter) =>
      acc.updated(
        adapter,
        (Math.max(0, adapter - 3) until adapter).map(acc.getOrElse(_, 0L)).sum
      )
  }(input.max))
}
