package adventofcode.day1

object Part2 {

  def run(input: String): Unit = {
    val chars = input.split("")
    var acc = 0
    for (i <- 0 until chars.length) {
      val char = chars(i)
      if ("(".equals(char)) acc += 1
      else acc -= 1

      if (acc == -1) {
        println("Position: " + (i + 1))
        return
      }
    }
  }
}
