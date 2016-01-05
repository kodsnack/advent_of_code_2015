package adventofcode.day3

object Part1 {

  def run(input: String): Unit = {
    val directions = parseInput(input)
    var x = 0
    var y = 0

    val grid = directions.foldLeft(Vector("0:0")) { (g, direction) =>
      if ("^" == direction) y += 1
      else if (">" == direction) x += 1
      else if ("v" == direction) y -= 1
      else if ("<" == direction) x -= 1
      else throw new RuntimeException(s"Invalid input: $direction")

      val pos = s"$x:$y"
      if (g.contains(pos)) g
      else g :+ pos
    }

    println(s"At least one: ${grid.size}")
  }
}
