import scala.io.Source

object day14 extends App {
  val input = Source
    .fromFile("day14input.txt")
    .getLines()
    .map(_.split(" = "))
    .map(t => (t(0), t(1)))
    .toVector

  def binaryStringToLong(s: String): Long =
    s.reverse.zipWithIndex
      .filter(_._1 == '1')
      .map(t => Math.pow(2, t._2).toLong)
      .sum

  val (_, _, mem) = input.foldLeft((0L, 0L, Map[Int, Long]())) {
    case ((_, _, mem), ("mask", arg)) =>
      val newMaskAnd =
        binaryStringToLong(arg.replaceAll("[01]", "0").replace("X", "1"))
      val newMaskAdd = binaryStringToLong(arg.replace("X", "0"))
      (newMaskAnd, newMaskAdd, mem)

    case ((maskAnd, maskAdd, mem), (memIndex, arg)) =>
      val index = memIndex.dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val value = (arg.toLong & maskAnd) + maskAdd
      (maskAnd, maskAdd, mem + (index -> value))
  }
  println(mem.values.sum)

  // 2 star
  def allAddresses(address: Int, mask: String): Vector[Long] = {
    val floatingIndices =
      mask.reverse.zipWithIndex.filter(_._1 == 'X').map(_._2)
    val maskedAddress = binaryStringToLong(
      address.toBinaryString.reverse
        .padTo(36, '0')
        .reverse
        .zip(mask)
        .map {
          case (ab, '0') => ab
          case (_, '1')  => '1'
          case (_, 'X')  => '0'
        }
        .mkString("")
    )
    (0L until Math.pow(2, floatingIndices.length).toLong)
      .map(offset => {
        maskedAddress + offset.toBinaryString.reverse
          .padTo(floatingIndices.length, '0')
          .reverse
          .zip(floatingIndices)
          .map {
            case ('1', p) => Math.pow(2, p).toLong
            case _        => 0
          }
          .sum
      })
      .toVector
  }

  val (_, mem2) = input.foldLeft(("", Map[Long, Long]())) {
    case ((_, mem), ("mask", arg)) =>
      (arg, mem)

    case ((mask, mem), (memIndex, arg)) =>
      val index = memIndex.dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val value = arg.toLong
      (mask, mem ++ allAddresses(index, mask).map((_ -> value)))
  }
  println(mem2.values.sum)
}
