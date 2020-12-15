import scala.annotation.tailrec
import scala.io.Source

object day15 extends App {
  val input = Source
    .fromFile("day15input.txt")
    .getLines()
    .next
    .split(",")
    .map(_.toInt)
    .toVector

  def getNthWord(input: Vector[Int], n: Int): Int = {
    @tailrec
    def loop(spokenWords: Map[Int, (Option[Int], Option[Int])],
             turnNum: Int,
             lastWord: Int): Int = {
      val newWord = spokenWords.get(lastWord) match {
        case Some((Some(last), Some(prev))) => last - prev
        case _                              => 0
      }
      if (turnNum == n) newWord
      else {
        val last = spokenWords.get(newWord).flatMap(_._1)
        val newSpokenWords = spokenWords.updated(newWord, (Some(turnNum), last))
        loop(newSpokenWords, turnNum + 1, newWord)
      }
    }
    loop(input.zipWithIndex.map {
      case (word, t) => word -> (Some(t + 1), None)
    }.toMap, input.length + 1, input.last)
  }
  println(getNthWord(input, 2020))
  // 2 star
  println(getNthWord(input, 30000000))
}
