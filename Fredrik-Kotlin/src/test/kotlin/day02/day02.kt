package test.kotlin.day02

import org.junit.Test
import test.util.loadInputAsLineList
import kotlin.test.assertTrue

class DayTwo {
    val filename = "day02.input"
    val filepath = "src/test/kotlin/day02/" + filename

    @Test fun test_partOne() {
        val input = loadInputAsLineList(filename, filepath)
        // Formatted as lxwxh (e.g. 2x5x8)
        var sqFeetSum = 0
        input.forEach {
            val dimens = it.split("x")
            val l = dimens[0].toInt()
            val w = dimens[1].toInt()
            val h = dimens[2].toInt()

            val surfacelw = 2*l*w
            val surfacewh = 2*w*h
            val surfacehl = 2*h*l
            val smallestSide = Math.min(Math.min(l*w, w*h), h*l)
            sqFeetSum += surfacelw + surfacewh + surfacehl + smallestSide
        }
        assertTrue { sqFeetSum == 1598415 }
    }

    @Test fun test_partTwo() {
        val input = loadInputAsLineList(filename, filepath)
        // Formatted as lxwxh (e.g. 2x5x8)
        var feetSum = 0
        input.forEach {
            val dimens = it.split("x")
            val intList = dimens.map { it.toInt() }
            val sortedList = intList.sorted()

            val first = sortedList[0]
            val middle = sortedList[1]
            val last = sortedList[2]
            val ribbonWrap = first + first + middle + middle
            val ribbonBow = first * middle * last
            feetSum += ribbonWrap + ribbonBow
        }
        assertTrue { feetSum == 3812909 }
    }
}


