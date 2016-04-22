import java.io.FileInputStream
import java.io.InputStreamReader

import junit.framework.Assert
import org.junit.Test
import java.io.File
import java.io.FileNotFoundException

class DayOne {
    @Test fun test_partOne() {
        val partOneResult = partOne()
        assert(partOneResult == 232)
    }

    fun partOne() : Int{
        val floors = loadInput()
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
        val floors = loadInput()
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

fun loadInput() : String {
    val filename = "day01.input"
    val filepath = "src/test/day01/" + filename
    var inputFile: File
    when {
        File(filename).exists() -> inputFile = File(filename)
        File(filepath).exists() -> inputFile = File(filepath)
        else -> throw FileNotFoundException("$filename nor $filepath was found")
    }

    val fis = FileInputStream(inputFile).buffered()
    var floors = ""
    try {
        val isr = InputStreamReader(fis)
        isr.readLines().forEach { floors += it }
    } finally {
        fis.close()
    }
    return floors
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

