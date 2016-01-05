package adventofcode

package object day2 {

  def parseInput(input: String): Array[String] = input.trim.split("\n")

  val pattern = """(\d+)x(\d+)x(\d+)""".r
}
