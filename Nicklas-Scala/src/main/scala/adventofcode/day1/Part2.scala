package adventofcode.day1

object Part2 {

  def run(input: String): Unit = {
    val chars = parseInput(input)
    var acc = 0
    for (i <- chars.indices) {
      val char = chars(i)
      if ("(" == char) acc += 1
      else acc -= 1

      if (acc == -1) {
        println(s"Position: ${i + 1}")
        return
      }
    }
  }
}
