import scala.io.Source

object day6 extends App {
  val input = Source.fromFile("day6input.txt").getLines().mkString("\n")
  val groups = input.split(raw"\R\R").toVector

  println(groups.map(_.filter(_.isLetter).distinct.length).sum)

  // 2 star
  println(
    groups
      .map(group => {
        group.split(raw"\R").map(_.toSet).reduce(_.intersect(_)).size
      })
      .sum
  )
}
