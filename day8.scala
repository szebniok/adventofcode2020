import scala.annotation.tailrec
import scala.io.Source

object day8 extends App {
  val input = Source.fromFile("day8input.txt").getLines().toVector

  @tailrec
  def run(program: Vector[String],
          pc: Int = 0,
          acc: Int = 0,
          visited: Set[Int] = Set()): Int =
    if (visited.contains(pc)) acc
    else {
      val newVisited = visited + pc
      program(pc).split(" ") match {
        case Array("acc", arg) =>
          run(program, pc + 1, acc + arg.toInt, newVisited)
        case Array("jmp", arg) =>
          run(program, pc + arg.toInt, acc, newVisited)
        case Array("nop", _) =>
          run(program, pc + 1, acc, newVisited)
      }
    }

  println(run(input))

  // 2 star
  def terminate(program: Vector[String],
                pc: Int = 0,
                acc: Int = 0,
                visited: Set[Int] = Set(),
                fixed: Boolean = false): Option[Int] =
    if (pc == program.length) Some(acc)
    else if (pc > program.length || visited.contains(pc)) None
    else {
      val newVisited = visited + pc
      program(pc).split(" ") match {
        case Array("acc", arg) =>
          terminate(program, pc + 1, acc + arg.toInt, newVisited, fixed)

        case Array("jmp", arg) =>
          terminate(program, pc + arg.toInt, acc, newVisited, fixed).orElse(
            if (fixed) None
            else terminate(program, pc + 1, acc, newVisited, true)
          )

        case Array("nop", arg) =>
          terminate(program, pc + 1, acc, newVisited, fixed).orElse(
            if (fixed) None
            else terminate(program, pc + arg.toInt, acc, newVisited, true)
          )
      }
    }
  println(terminate(input).get)
}
