import scala.io.Source

object day5 extends App {
  val input = Source.fromFile("day5input.txt").getLines().toVector

  val ids = input.map(seat => {
    val row = seat
      .take(7)
      .zipWithIndex
      .filter(_._1 == 'B')
      .map {
        case (_, i) =>
          Math.pow(2, 7 - i - 1)
      }
      .sum
    val column = seat
      .drop(7)
      .zipWithIndex
      .filter(_._1 == 'R')
      .map {
        case (_, i) => Math.pow(2, 3 - i - 1)
      }
      .sum
    (row * 8 + column).toInt
  })
  println(ids.max)

  // 2 star
  val idsSet = ids.toSet
  println((ids.min to ids.max).filter(!idsSet.contains(_)).head)
}
