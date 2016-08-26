package day03

import org.junit.Test
import test.util.loadInputAsLines
import java.util.*
import kotlin.test.assertTrue

class DayThree {
    val filename = "day03.input"
    val filepath = "src/test/kotlin/day03/" + filename

    @Test fun test_partOne() {
        val input = loadInputAsLines(filename, filepath)
        val visitedHouses = HashSet<Point>()
        val santa = Point(0, 0)
        visitedHouses.add(santa)
        input.forEach {
            when {
                it.equals('>') -> santa.x += 1
                it.equals('<') -> santa.x -= 1
                it.equals('^') -> santa.y += 1
                it.equals('v') -> santa.y -= 1
                else -> throw IllegalArgumentException("Character not recognized: " + it)
            }
            visitedHouses.add(santa.copy())
        }
        assertTrue(message = visitedHouses.size.toString()) { visitedHouses.size == 2572 }
    }

    @Test fun test_partTwo() {
        val input = loadInputAsLines(filename, filepath)
        val visitedHouses = HashSet<Point>()
        val santa = Point(0, 0)
        val roboSanta = Point(0, 0)
        var currentSanta: Point
        visitedHouses.add(roboSanta)
        input.forEachIndexed { index, direction ->
            currentSanta = if (index % 2 == 0) santa else roboSanta
            when {
                direction.equals('>') -> currentSanta.x += 1
                direction.equals('<') -> currentSanta.x -= 1
                direction.equals('^') -> currentSanta.y += 1
                direction.equals('v') -> currentSanta.y -= 1
                else -> throw IllegalArgumentException("Character not recognized: " + direction)
            }
            visitedHouses.add(currentSanta.copy())
        }
        assertTrue(message = visitedHouses.size.toString()) { visitedHouses.size == 2631 }
    }
}

data class Point(var x: Int, var y: Int) {
    override fun hashCode(): Int {
        return x * 31 + y * 17
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other?.javaClass != javaClass) return false

        other as Point

        if (x != other.x) return false
        if (y != other.y) return false

        return true
    }
}