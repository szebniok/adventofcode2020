import scala.annotation.tailrec
import scala.io.Source

object day9 extends App {
  val input = Source.fromFile("day9input.txt").getLines().map(_.toLong).toVector
  val (preamble, rest) = input.splitAt(25)

  def pairExists(preamble: Vector[Long], n: Long): Boolean = {
    @tailrec
    def loop(low: Int, high: Int): Boolean = {
      if (high <= low) false
      else {
        val sum = preamble(low) + preamble(high)
        if (sum == n) true
        else if (sum > n) loop(low, high - 1)
        else loop(low + 1, high)
      }
    }
    loop(0, preamble.length - 1)
  }

  @tailrec
  def firstAbsent(preamble: Vector[Long], rest: Vector[Long]): Long = {
    val next = rest.head
    if (!pairExists(preamble.sorted, next)) next
    else firstAbsent(preamble.tail :+ next, rest.tail)
  }

  val star1 = firstAbsent(preamble, rest)
  println(star1)

  // 2 star
  @tailrec
  def contiguousSum(input: Vector[Long],
                    target: Long,
                    terms: Vector[Long] = Vector(),
                    sum: Long = 0): Long =
    if (target == sum) terms.min + terms.max
    else if (target < sum)
      contiguousSum(input, target, terms.tail, sum - terms.head)
    else
      contiguousSum(input.tail, target, terms :+ input.head, sum + input.head)
  println(contiguousSum(input, star1))
}
