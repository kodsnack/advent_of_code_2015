package adventofcode.day3

object Part2 {

  def run(input: String): Unit = {
    val directions = parseInput(input)
    var grid = Vector("0:0")
    var sx = 0
    var sy = 0
    var rx = 0
    var ry = 0

    def addPos(x: Int, y: Int, direction: String): Unit = {
      val pos = s"$x:$y"
      if (!grid.contains(pos)) {
        grid = grid :+ pos
      }
    }

    for (i <- directions.indices) {
      val direction = directions(i)
      if (i % 2 == 0) {
        if ("^" == direction) sy += 1
        else if (">" == direction) sx += 1
        else if ("v" == direction) sy -= 1
        else if ("<" == direction) sx -= 1
        else throw new RuntimeException(s"Invalid input: $direction")

        addPos(sx, sy, direction)
      } else {
        if ("^" == direction) ry += 1
        else if (">" == direction) rx += 1
        else if ("v" == direction) ry -= 1
        else if ("<" == direction) rx -= 1
        else throw new RuntimeException(s"Invalid input: $direction")

        addPos(rx, ry, direction)
      }
    }
    
    println(s"Robo-Santa: ${grid.size}")
  }
}
