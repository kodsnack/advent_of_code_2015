package day05

import org.junit.Test
import test.util.loadInputAsLineList

class DayFive  {
    val filename = "day05.input"
    val filepath = "src/test/kotlin/day05/" + filename
    val lineList = loadInputAsLineList(filename, filepath)

    @Test fun test_partOne() {
        val vowelsPattern = Regex("[aeiou]")
        val twicePattern = Regex("([a-z])\\1{1,}")
        val pairPattern = Regex("(ab)|(cd)|(pq)|(xy)")
        var niceLines = 0
        for (line in lineList) {
            val foundVowels = vowelsPattern.containsMatchIn(line)
            val foundTwice = twicePattern.containsMatchIn(line)
            val foundInvalidPair = pairPattern.containsMatchIn(line)
            if (foundVowels && foundTwice && !foundInvalidPair) {
                if (vowelsPattern.findAll(line).count() < 3) {
                    continue
                }
                niceLines++
            }
        }
        assert(niceLines == 238)
    }

    @Test fun test_partTwo() {
        val pairPattern = Regex("(..).*\\1")
        val letterRepeatPattern = Regex("(.).\\1")
        var niceLines = 0
        for (line in lineList) {
            if (!pairPattern.containsMatchIn(line)) {
                continue
            }
            if (!letterRepeatPattern.containsMatchIn(line)) {
                continue
            }
            niceLines++
        }
        assert(niceLines == 69)
    }
}