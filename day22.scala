import scala.annotation.tailrec
import scala.io.Source

object day22 extends App {
  val input = Source.fromFile("day22input.txt").getLines().toList
  val (player1, player2) = input.span(_.nonEmpty) match {
    case (p1, p2) =>
      (p1.flatMap(_.toIntOption), p2.flatMap(_.toIntOption))
  }

  @tailrec
  def getWinnerDeck(player1: List[Int], player2: List[Int]): List[Int] =
    (player1, player2) match {
      case (p1, Nil) => p1
      case (Nil, p2) => p2
      case (card1 :: deck1, card2 :: deck2) =>
        if (card1 > card2)
          getWinnerDeck(deck1 ++ List(card1, card2), deck2)
        else
          getWinnerDeck(deck1, deck2 ++ List(card2, card1))
    }

  println(getWinnerDeck(player1, player2).reverse.zipWithIndex.map {
    case (card, i) => card * (i + 1)
  }.sum)

  // 2 star
  def getRecursiveWinner(player1: List[Int],
                         player2: List[Int],
                         seen: Set[(List[Int], List[Int])]): (Int, List[Int]) =
    if (seen.contains((player1, player2))) (1, player1)
    else {
      val newSeen = seen + ((player1, player2))
      (player1, player2) match {
        case (p1, Nil) => (1, p1)
        case (Nil, p2) => (2, p2)
        case (card1 :: deck1, card2 :: deck2) =>
          if (card1 <= deck1.length && card2 <= deck2.length) {
            val (winner, _) =
              getRecursiveWinner(deck1.take(card1), deck2.take(card2), newSeen)
            if (winner == 1)
              getRecursiveWinner(deck1 ++ List(card1, card2), deck2, newSeen)
            else
              getRecursiveWinner(deck1, deck2 ++ List(card2, card1), newSeen)
          } else {
            if (card1 > card2)
              getRecursiveWinner(deck1 ++ List(card1, card2), deck2, newSeen)
            else
              getRecursiveWinner(deck1, deck2 ++ List(card2, card1), newSeen)
          }
      }
    }
  println(
    getRecursiveWinner(player1, player2, Set())._2.reverse.zipWithIndex.map {
      case (card, i) => card * (i + 1)
    }.sum
  )
}
