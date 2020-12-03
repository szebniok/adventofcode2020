import scala.io.Source

object day3 extends App {
  val input = Source.fromFile("day3input.txt").getLines().toVector

  println(input.zipWithIndex.count {
    case (line, y) =>
      val x = (y * 3) % line.length
      line(x) == '#'
  })

  // 2 star
  val slopes = Vector((1, 1), (1, 3), (1, 5), (1, 7), (2, 1))
  println(slopes.map {
    case (down, right) =>
      input.grouped(down).map(_.head).zipWithIndex.count {
        case (line, y) =>
          val x = (y * right) % line.length
          line(x) == '#'
      }
  }.product)
}
