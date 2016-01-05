package adventofcode.day1

object Part1 {

  def run(input: String): Unit = {
    println("Floor: " + parseInput(input)
      .foldLeft(0) { (acc, char) =>
        if ("(" == char) acc + 1
        else acc - 1
      })
  }
}
