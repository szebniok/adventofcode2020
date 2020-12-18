import scala.annotation.tailrec
import scala.io.Source

object day18 extends App {
  val input = Source
    .fromFile("day18input.txt")
    .getLines()
    .map(_.filter(!_.isWhitespace))
    .toVector

  // Expression -> Term {Op Term}
  // Term -> int
  // Term -> '(' Expression ')'
  // Op -> '+' | '*'
  def expression(input: String): (BigInt, String) = {
    @tailrec
    def loop(acc: BigInt, rest: String): (BigInt, String) =
      if (!rest.headOption.exists(Set('+', '*').contains)) (acc, rest)
      else {
        val op: (BigInt, BigInt) => BigInt =
          if (rest.head == '+') _ + _ else _ * _
        val t = term(rest.tail)
        loop(op(acc, t._1), t._2)
      }
    (loop _).tupled(term(input))
  }
  def term(input: String): (BigInt, String) = {
    if (input.head.isDigit) {
      val (value, rest) = input.span(_.isDigit)
      (value.toInt, rest)
    } else {
      val (value, rest) = expression(input.tail)
      (value, rest.tail)
    }
  }
  println(input.map(line => expression(line)._1).sum)

  // 2 star
  // Mul -> Add {'*' Add}
  // Add -> Term2 {'+' Term2}
  // Term2 -> int
  // Term2 -> '(' Mul ')'
  def mul(input: String): (BigInt, String) = {
    @tailrec
    def loop(acc: BigInt, rest: String): (BigInt, String) =
      if (!rest.headOption.contains('*')) (acc, rest)
      else {
        val a = add(rest.tail)
        loop(acc * a._1, a._2)
      }
    (loop _).tupled(add(input))
  }
  def add(input: String): (BigInt, String) = {
    @tailrec
    def loop(acc: BigInt, rest: String): (BigInt, String) =
      if (!rest.headOption.contains('+')) (acc, rest)
      else {
        val t = term2(rest.tail)
        loop(acc + t._1, t._2)
      }
    (loop _).tupled(term2(input))
  }
  def term2(input: String): (BigInt, String) = {
    if (input.head.isDigit) {
      val (value, rest) = input.span(_.isDigit)
      (value.toInt, rest)
    } else {
      val (value, rest) = mul(input.tail)
      (value, rest.tail)
    }
  }
  println(input.map(line => mul(line)._1).sum)
}
