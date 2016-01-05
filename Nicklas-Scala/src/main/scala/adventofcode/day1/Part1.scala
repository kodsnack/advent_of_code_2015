package adventofcode.day1

object Part1 {

  def run(input: String): Unit = {
    println("Floor: " + input.split("")
      .foldLeft(0) { (acc, char) =>
        if ("(".equals(char)) acc + 1
        else acc - 1
      })
  }
}
