import scala.io.Source

object day12 extends App {
  val input = Source.fromFile("day12input.txt").getLines().toVector
  val actions = input.map(_.splitAt(1)).map { case (a, b) => (a, b.toInt) }
  val (x1, y1, _) = actions.foldLeft((0, 0, 90)) {
    case ((x, y, rotation), (action, value)) =>
      action match {
        case "N" => (x, y + value, rotation)
        case "S" => (x, y - value, rotation)
        case "E" => (x + value, y, rotation)
        case "W" => (x - value, y, rotation)
        case "L" => (x, y, Math.floorMod(rotation - value, 360))
        case "R" => (x, y, Math.floorMod(rotation + value, 360))
        case "F" =>
          val dx = if (rotation == 90) 1 else if (rotation == 270) -1 else 0
          val dy = if (rotation == 0) 1 else if (rotation == 180) -1 else 0
          (x + value * dx, y + value * dy, rotation)
      }
  }
  println(Math.abs(x1) + Math.abs(y1))

  // 2 star
  val (x2, y2, _, _) = actions.foldLeft((0, 0, 10, 1)) {
    case ((x, y, wx, wy), (action, value)) =>
      action match {
        case "N" => (x, y, wx, wy + value)
        case "S" => (x, y, wx, wy - value)
        case "E" => (x, y, wx + value, wy)
        case "W" => (x, y, wx - value, wy)
        case lr if "LR".contains(lr) =>
          val (newWX, newWY) = (if (lr == "R") 360 - value else value) match {
            case 90  => (-wy, wx)
            case 180 => (-wx, -wy)
            case 270 => (wy, -wx)
            case _   => (wx, wy)
          }
          (x, y, newWX, newWY)
        case "F" =>
          (x + value * wx, y + value * wy, wx, wy)
      }
  }
  println(Math.abs(x2) + Math.abs(y2))
}
