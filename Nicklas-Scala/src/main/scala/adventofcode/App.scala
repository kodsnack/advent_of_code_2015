package adventofcode

import scala.io.Source

object App {

  def main(args: Array[String]): Unit = {
    day1.Part1.run(input1)
    day1.Part2.run(input1)
    day2.Part1.run(input2)
    day2.Part2.run(input2)
    day3.Part1.run(input3)
    day3.Part2.run(input3)
  }

  val input1 = readFile("day1.txt")
  val input2 = readFile("day2.txt")
  val input3 = readFile("day3.txt")

  private def readFile(name: String): String = {
    val stream = getClass.getResourceAsStream(s"/$name")
    Source.fromInputStream(stream).mkString
  }
}
