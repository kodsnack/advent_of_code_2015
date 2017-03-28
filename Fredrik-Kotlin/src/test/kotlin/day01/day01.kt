package day01

import org.junit.Test
import test.util.loadInputAsLines

class DayOne {
    val filename = "day01.input"
    val filepath = "src/test/kotlin/day01/" + filename

    @Test fun test_partOne() {
        val partOneResult = partOne()
        assert(partOneResult == 232)
    }

    fun partOne() : Int {
        val floors = loadInputAsLines(filename, filepath)
        var sum = 0
        floors.forEach {
            sum += it.toFloor()
        }
        return sum
    }

    @Test fun test_partTwo() {
        val partTwoResult = partTwo()
        assert(partTwoResult == 1783)
    }

    fun partTwo() : Int {
        val floors = loadInputAsLines(filename, filepath)
        var sum = 0
        // Index 0 equals Floor 1
        val positionOffset = 1
        floors.forEachIndexed { index, value ->
            sum += value.toFloor()
            if (sum == -1) {
                return index + positionOffset
            }
        }
        return 0
    }
}

fun Char.toFloor() : Int {
    if (this.equals(')')) {
        return -1
    } else if (this.equals('(')) {
        return 1
    }
    throw IllegalArgumentException("Illegal floor type, " +
            "must be either ')' or '('")
}

