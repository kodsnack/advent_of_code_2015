package day04

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

@RunWith(Parameterized::class)
class DayFour(val hashStart: String, val expectedNumber: Int) {
    val md5 = MessageDigest.getInstance("MD5")

    companion object {
        @JvmStatic
        @Parameterized.Parameters
        fun data() : Collection<Array<Any>> {
            return listOf(
                    arrayOf("00000", 346386),
                    arrayOf("000000", 9958218)
            )
        }
    }

    @Test fun test_findLowestNumber() {
        assert(findLowestNumber(hashStart) == expectedNumber)
    }

    fun findLowestNumber(startingCondition: String) : Int {
        val secret = "iwrupvqb"
        var count = 0
        var hash: String
        do {
            count++
            hash = calculateHash(secret + count)
        } while (!hash.startsWith(startingCondition, true))

        return count
    }

    fun calculateHash(candidate: String) : String {
        val digest = md5.digest(candidate.toByteArray())
        return DatatypeConverter.printHexBinary(digest)
    }
}