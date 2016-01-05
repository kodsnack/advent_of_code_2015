package adventofcode.day2

object Part1 {

  def run(input: String): Unit = {
    val dims = parseInput(input)

    val required = dims.foldLeft(0) { (acc, size) =>
      size match {
        case pattern(ls, ws, hs) =>
          val l = Integer.parseInt(ls)
          val w = Integer.parseInt(ws)
          val h = Integer.parseInt(hs)

          val side1 = l * w
          val side2 = w * h
          val side3 = h * l

          val least = Integer.min(Integer.min(side1, side2), side3)

          acc + (2 * side1) + (2 * side2) + (2 * side3) + least
      }
    }

    println(s"Required: $required")
  }
}
