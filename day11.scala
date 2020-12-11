import scala.annotation.tailrec
import scala.io.Source

object day11 extends App {
  val input = Source.fromFile("day11input.txt").getLines().toVector
  val grid = input.map(_.toCharArray.toVector)

  @tailrec
  def occupiedSeats(grid: Vector[Vector[Char]]): Int = {
    val newGrid = grid.zipWithIndex.map {
      case (xs, y) =>
        xs.zipWithIndex.map {
          case (seat, x) =>
            val neighbours = for {
              ny <- y - 1 to y + 1
              nx <- x - 1 to x + 1
              if 0 <= ny && ny < grid.length
              if 0 <= nx && nx < grid(0).length
              if (ny, nx) != (y, x)
            } yield grid(ny)(nx)

            if (seat == 'L' && !neighbours.contains('#')) '#'
            else if (seat == '#' && neighbours.count(_ == '#') >= 4) 'L'
            else seat
        }
    }

    if (newGrid == grid) newGrid.map(_.count(_ == '#')).sum
    else occupiedSeats(newGrid)
  }
  println(occupiedSeats(grid))

  // 2 star
  @tailrec
  def occupiedSeats2(grid: Vector[Vector[Char]]): Int = {
    val newGrid = grid.zipWithIndex.map {
      case (xs, y) =>
        xs.zipWithIndex.map {
          case (seat, x) =>
            val neighbours = for {
              dy <- -1 to 1
              dx <- -1 to 1
              if (dy, dx) != (0, 0)
              firstSeat = LazyList
                .from(1)
                .map(dist => (y + dist * dy, x + dist * dx))
                .takeWhile {
                  case (ny, nx) =>
                    0 <= ny && ny < grid.length && 0 <= nx && nx < grid(0).length
                }
                .map { case (ny, nx) => grid(ny)(nx) }
                .find(_ != '.')
                .getOrElse('.')
            } yield firstSeat

            if (seat == 'L' && !neighbours.contains('#')) '#'
            else if (seat == '#' && neighbours.count(_ == '#') >= 5) 'L'
            else seat
        }
    }

    if (newGrid == grid) newGrid.map(_.count(_ == '#')).sum
    else occupiedSeats2(newGrid)
  }
  println(occupiedSeats2(grid))
}
