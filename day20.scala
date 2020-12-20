import scala.io.Source

object day20 extends App {
  val input = Source.fromFile("day20input.txt").getLines().toVector

  case class Tile(id: Int, pattern: Vector[Vector[Char]])

  val tiles = input.grouped(12).toVector.map {
    case header +: encodedPattern =>
      val id = header.dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val pattern = encodedPattern.init.map(_.toVector)
      Tile(id, pattern)
  }

  def flipVertical(tile: Tile): Tile =
    tile.copy(pattern = tile.pattern.reverse)

  def rotateLeft(tile: Tile): Tile = {
    val n = tile.pattern.length - 1
    val newPattern = (for {
      x <- n to 0 by -1
      y <- 0 to n
    } yield (y, x))
      .map { case (y, x) => tile.pattern(y)(x) }
      .grouped(n + 1)
      .map(_.toVector)
      .toVector
    tile.copy(pattern = newPattern)
  }

  val permutedTiles = tiles.flatMap(tile => {
    val t1 = tile
    val t2 = flipVertical(t1)
    val t3 = rotateLeft(t1)
    val t4 = flipVertical(t3)
    val t5 = rotateLeft(t3)
    val t6 = flipVertical(t5)
    val t7 = rotateLeft(t5)
    val t8 = flipVertical(t7)
    Vector(t1, t2, t3, t4, t5, t6, t7, t8)
  })

  val topEdges = permutedTiles.groupBy(_.pattern.head)
  val leftEdges = permutedTiles.groupBy(_.pattern.map(_.head))

  def fillTiles(
    x: Int,
    y: Int,
    usedTiles: Set[Int] = Set(),
    currentFill: Map[(Int, Int), Tile] = Map()
  ): Option[Map[(Int, Int), Tile]] = {
    if (usedTiles.size == tiles.size) Some(currentFill)
    else if ((x, y) == (0, 0)) {
      LazyList
        .from(permutedTiles)
        .map(firstTile => {
          val newCurrentFill = Map((0, 0) -> firstTile)
          val newUsedTIles = Set(firstTile.id)
          fillTiles(1, 0, newUsedTIles, newCurrentFill)
        })
        .find(_.nonEmpty)
        .flatten
    } else {
      val leftNeighbourEdge =
        currentFill.get((x - 1, y)).map(_.pattern.map(_.last))
      val topNeighbourEdge =
        currentFill.get((x, y - 1)).map(_.pattern.last)

      val leftConstraint = leftNeighbourEdge.flatMap(
        left =>
          leftEdges
            .get(left)
            .map(_.filter(t => !usedTiles.contains(t.id)))
      )
      val topConstraint = topNeighbourEdge.flatMap(
        top =>
          topEdges
            .get(top)
            .map(_.filter(t => !usedTiles.contains(t.id)))
      )

      val tile = (leftConstraint, topConstraint) match {
        case (Some(possibleLeft), Some(possibleRight)) =>
          val possibleTile = possibleLeft.filter(possibleRight.contains)
          possibleTile.headOption
        case (Some(possibleLeft), None) =>
          possibleLeft.headOption
        case (None, Some(possibleTop)) =>
          possibleTop.headOption
        case _ => None
      }

      tile.flatMap(t => {
        val newX = if (x == 11) 0 else x + 1
        val newY = if (newX == 0) y + 1 else y
        fillTiles(newX, newY, usedTiles + t.id, currentFill + ((x, y) -> t))
      })
    }
  }

  val Some(filling) = fillTiles(0, 0)

  println(
    Vector((0, 0), (11, 0), (0, 11), (11, 11))
      .map(filling)
      .map(_.id.toLong)
      .product
  )

  // 2 star
  val pictureTiles = for {
    y <- 0 to 11
    x <- 0 to 11
  } yield filling((x, y))

  val picture = Tile(
    0,
    pictureTiles
      .grouped(12)
      .map(
        _.map(_.pattern.tail.init.map(_.tail.init)).toVector
          .reduceLeft[Vector[Vector[Char]]] {
            case (acc, t) => acc.zip(t).map { case (a, b) => a ++ b }
          }
      )
      .reduceLeft[Vector[Vector[Char]]] {
        case (acc, row) => acc ++ row
      }
  )

  val permutedPictures = {
    val p1 = picture
    val p2 = flipVertical(p1)
    val p3 = rotateLeft(p1)
    val p4 = flipVertical(p3)
    val p5 = rotateLeft(p3)
    val p6 = flipVertical(p5)
    val p7 = rotateLeft(p5)
    val p8 = flipVertical(p7)
    Vector(p1, p2, p3, p4, p5, p6, p7, p8)
  }

  val monster = "..................#.#....##....##....###.#..#..#..#..#..#..."
  val monsterRegex = monster.r

  val pictureLocations =
    permutedPictures.map(
      p =>
        (
          p,
          p.pattern.zipWithIndex
            .sliding(3)
            .flatMap {
              case Vector((topRow, y), (middleRow, _), (bottomRow, _)) =>
                (0 until picture.pattern.length - 20).map(offset => {
                  (
                    offset,
                    y,
                    Vector(topRow, middleRow, bottomRow)
                      .flatMap(_.slice(offset, offset + 20))
                      .mkString("")
                  )
                })
            }
            .filter(t => monsterRegex.matches(t._3))
            .map { case (x, y, _) => (x, y) }
      )
    )
  val (finalPicture, locations) = pictureLocations.filter(_._2.nonEmpty).head
  println(
    finalPicture.pattern
      .map(_.count(_ == '#'))
      .sum - locations.length * monster.count(_ == '#')
  )
}
