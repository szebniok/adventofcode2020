import scala.io.Source

object day13 extends App {
  val input = Source.fromFile("day13input.txt").getLines().toVector
  val earliestDepart = input(0).toInt
  val busIds = input(1).split(",").flatMap(_.toIntOption)

  val earliestBus = busIds.minBy(id => id - (earliestDepart % id))
  println(earliestBus * (earliestBus - (earliestDepart % earliestBus)))

  // 2 star
  val busIdsAndOffsets = input(1)
    .split(",")
    .zipWithIndex
    .filter(_._1 != "x")
    .map { case (id, i) => (BigInt(id), BigInt(id.toLong - i.toLong)) }
    .toVector

  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
  def extendedGcd(a: BigInt, b: BigInt): (BigInt, BigInt) = {
    var (oldR, r) = (a, b)
    var (oldS, s) = (BigInt(1), BigInt(0))
    var (oldT, t) = (BigInt(0), BigInt(1))

    while (r != 0) {
      val quotient = oldR / r

      var tmp = r
      r = oldR - quotient * tmp
      oldR = tmp

      tmp = s
      s = oldS - quotient * tmp
      oldS = tmp

      tmp = t
      t = oldT - quotient * tmp
      oldT = tmp
    }
    (oldS, oldT)
  }

  println(
    busIdsAndOffsets
      .reduceLeft((p1, p2) => {
        val (n1, a1) = p1
        val (n2, a2) = p2
        val (m1, m2) = extendedGcd(n1, n2)
        var x = a1 * n2 * m2 + a2 * n1 * m1
        val newN = n1 * n2
        x = x % newN
        (newN, x)
      })
      ._2
  )
}
