import scala.io.Source

object day19 extends App {
  val input = Source.fromFile("day19input.txt").getLines().toVector
  val rulesInput = input.takeWhile(_.nonEmpty)
  val messages = input.dropWhile(_.nonEmpty).tail

  sealed trait Rule
  case class Node(children: Vector[Vector[Int]]) extends Rule
  case class Leaf(char: Char) extends Rule

  val rules = rulesInput
    .map(line => {
      val Array(id, rest) = line.split(": ")
      if (rest.startsWith("\"")) {
        id.toInt -> Leaf(rest.tail.head)
      } else {
        val children =
          rest.split(raw" \| ").map(_.split(" ").map(_.toInt).toVector).toVector
        id.toInt -> Node(children)
      }
    })
    .toMap

  def matchRule(text: String,
                id: Int,
                rules: Map[Int, Rule]): Option[Vector[String]] =
    if (text.isEmpty) None
    else
      rules(id) match {
        case Leaf(char) if char == text.head => Some(Vector(text.tail))
        case Leaf(_)                         => None
        case Node(children) =>
          Some(
            children
              .flatMap(_.foldLeft(Option(Vector(text))) {
                case (Some(Vector()), _) => None
                case (Some(rests), childId) =>
                  Some(rests.flatMap(r => matchRule(r, childId, rules)).flatten)
                case _ => None
              })
              .flatten
          ).filter(_.nonEmpty)
      }
  println(messages.flatMap(matchRule(_, 0, rules)).count(_.contains("")))

  // 2 star
  val updatedRules =
    rules ++ Vector(
      8 -> Node(Vector(Vector(42), Vector(42, 8))),
      11 -> Node(Vector(Vector(42, 31), Vector(42, 11, 31)))
    )
  println(messages.flatMap(matchRule(_, 0, updatedRules)).count(_.contains("")))
}
