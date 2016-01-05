package adventofcode.day2

object Part2 {

  def run(input: String): Unit = {
    val dims = parseInput(input)

    val required = dims.foldLeft(0) { (acc, size) =>
      size match {
        case pattern(ls, ws, hs) =>
          val l = Integer.parseInt(ls)
          val w = Integer.parseInt(ws)
          val h = Integer.parseInt(hs)

          val shortest1 = Integer.min(l, w)
          val shortest2 = Integer.min(if (shortest1 == l) w else l, h)

          val len = shortest1 * 2 + shortest2 * 2
          val bow = l * w * h

          acc + len + bow
      }
    }

    println(s"Ribbon: $required")
  }
}
