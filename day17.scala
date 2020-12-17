import scala.annotation.tailrec
import scala.io.Source

object day17 extends App {
  val input = Source
    .fromFile("day17input.txt")
    .getLines()
    .toVector
    .map(_.toCharArray.zipWithIndex)
    .zipWithIndex
    .flatMap { case (xs, y) => xs.map { case (c, x) => (x, y, 0) -> c } }
    .toMap

  @tailrec
  def getNthCycle(grid: Map[(Int, Int, Int), Char],
                  cyclesLeft: Int): Map[(Int, Int, Int), Char] =
    if (cyclesLeft == 0) grid
    else {
      val widthMin = grid.keys.map(_._1).min
      val widthMax = grid.keys.map(_._1).max
      val depthMin = grid.keys.map(_._3).min
      val depthMax = grid.keys.map(_._3).max

      val (newWidthMin, newWidthMax) = (widthMin - 1, widthMax + 1)
      val (newDepthMin, newDepthMax) = (depthMin - 1, depthMax + 1)
      val newIndices = for {
        x <- newWidthMin to newWidthMax
        y <- newWidthMin to newWidthMax
        z <- newDepthMin to newDepthMax
      } yield (x, y, z)

      val newGrid = newIndices.map {
        case (x, y, z) =>
          val neighbours = for {
            dx <- -1 to 1
            dy <- -1 to 1
            dz <- -1 to 1
            if (dx, dy, dz) != (0, 0, 0)
          } yield grid.getOrElse((x + dx, y + dy, z + dz), '.')

          val activeCount = neighbours.count(_ == '#')
          val newState = (grid.getOrElse((x, y, z), '.'), activeCount) match {
            case ('#', 2) => '#'
            case ('#', 3) => '#'
            case ('.', 3) => '#'
            case _        => '.'
          }
          (x, y, z) -> newState
      }.toMap
      getNthCycle(newGrid, cyclesLeft - 1)
    }
  println(getNthCycle(input, 6).count(_._2 == '#'))

  // 2 star
  @tailrec
  def getNthCycle4D(grid: Map[(Int, Int, Int, Int), Char],
                    cyclesLeft: Int): Map[(Int, Int, Int, Int), Char] =
    if (cyclesLeft == 0) grid
    else {
      val widthMin = grid.keys.map(_._1).min
      val widthMax = grid.keys.map(_._1).max
      val depthMin = grid.keys.map(_._3).min
      val depthMax = grid.keys.map(_._3).max

      val (newWidthMin, newWidthMax) = (widthMin - 1, widthMax + 1)
      val (newDepthMin, newDepthMax) = (depthMin - 1, depthMax + 1)
      val newIndices = for {
        x <- newWidthMin to newWidthMax
        y <- newWidthMin to newWidthMax
        z <- newDepthMin to newDepthMax
        w <- newDepthMin to newDepthMax
      } yield (x, y, z, w)

      val newGrid = newIndices.map {
        case (x, y, z, w) =>
          val neighbours = for {
            dx <- -1 to 1
            dy <- -1 to 1
            dz <- -1 to 1
            dw <- -1 to 1
            if (dx, dy, dz, dw) != (0, 0, 0, 0)
          } yield grid.getOrElse((x + dx, y + dy, z + dz, w + dw), '.')

          val activeCount = neighbours.count(_ == '#')
          val newState =
            (grid.getOrElse((x, y, z, w), '.'), activeCount) match {
              case ('#', 2) => '#'
              case ('#', 3) => '#'
              case ('.', 3) => '#'
              case _        => '.'
            }
          (x, y, z, w) -> newState
      }.toMap
      getNthCycle4D(newGrid, cyclesLeft - 1)
    }
  val input4d = input.map { case ((x, y, z), c) => (x, y, z, 0) -> c }
  println(getNthCycle4D(input4d, 6).count(_._2 == '#'))
}
