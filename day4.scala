import scala.io.Source

object day4 extends App {
  val input = Source.fromFile("day4input.txt").getLines().mkString("\n")
  val passports = input.split(raw"\R\R").toVector
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  println(passports.count(p => {
    val fields = p.split(raw"\s+").map(_.takeWhile(_ != ':')).toSet
    requiredFields.subsetOf(fields)
  }))

  // 2 star
  println(
    passports
      .map(p => {
        p.split(raw"\s+")
          .map(c => c.takeWhile(_ != ':') -> c.dropWhile(_ != ':').tail)
          .toMap
      })
      .filter(p => requiredFields.subsetOf(p.keys.toSet))
      .filter(_("byr").toIntOption.exists(year => 1920 <= year && year <= 2002))
      .filter(_("iyr").toIntOption.exists(year => 2010 <= year && year <= 2020))
      .filter(_("eyr").toIntOption.exists(year => 2020 <= year && year <= 2030))
      .filter(p => {
        val height = p("hgt")
        val value = height.takeWhile(_.isDigit).toInt
        val unit = height.dropWhile(_.isDigit)
        val (min, max) = if (unit == "cm") (150, 193) else (59, 76)
        min <= value && value <= max
      })
      .filter(_("hcl").matches("#[0-9a-f]{6}"))
      .filter(
        p =>
          Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
            .contains(p("ecl"))
      )
      .filter(_("pid").matches(raw"\d{9}"))
      .length
  )
}
