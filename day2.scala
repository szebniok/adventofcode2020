import scala.io.Source

object day2 extends App {
  val input = Source.fromFile("day2input.txt").getLines.toVector
  val lineRegex = raw"(\d+)-(\d+) (\w): (\w*)".r
  val parsedInput = input.map {
    case lineRegex(min, max, char, password) =>
      (min.toInt, max.toInt, char(0), password)
  }

  println(
    parsedInput
      .count {
        case (min, max, char, password) =>
          (min to max).contains(password.count(_ == char))
      }
  )

  // 2 star
  println(parsedInput.count {
    case (first, second, char, password) =>
      (password(first - 1) == char) ^ (password(second - 1) == char)
  })
}
