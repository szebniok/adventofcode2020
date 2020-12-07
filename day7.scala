import scala.io.Source

object day7 extends App {
  val input = Source.fromFile("day7input.txt").getLines().toVector
  val rules: Map[String, Vector[(String, Int)]] = input
    .map(line => {
      val words = line.split(" ")
      val color = words.take(2).mkString(" ")
      val contents = words
        .drop(4)
        .grouped(4)
        .filter(_.take(2).mkString(" ") != "no other")
        .map(group => {
          (group.tail.take(2).mkString(" "), group(0).toInt)
        })
        .toVector
      color -> contents
    })
    .toMap

  def canReachShinyGold(outermost: String): Boolean =
    if (outermost == "shiny gold") true
    else rules(outermost).map(_._1).exists(canReachShinyGold)

  println(
    rules.keys.toVector
      .filter(_ != "shiny gold")
      .count(canReachShinyGold)
  )

  // 2 star
  def howManyRequired(color: String, outermost: Boolean = true): Int = {
    rules(color).map {
      case (color, count) => count * howManyRequired(color, false)
    }.sum + (if (outermost) 0 else 1)
  }
  println(howManyRequired("shiny gold"))
}
