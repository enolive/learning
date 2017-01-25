import org.scalatest.{FlatSpec, Matchers}

class RomanDigitTest extends FlatSpec with Matchers {
    behavior of "Converting roman digits to arabic"

    it should "convert I to 1" in {
        arabicValueOf('I') should be(1)
    }

    it should "convert V to 5" in {
        arabicValueOf('V') should be(5)
    }

    it should "convert X to 10" in {
        arabicValueOf('X') should be(10)
    }

    it should "convert L to 50" in {
        arabicValueOf('L') should be(50)
    }

    it should "convert C to 100" in {
        arabicValueOf('C') should be(100)
    }

    it should "convert D to 500" in {
        arabicValueOf('D') should be(500)
    }

    it should "convert M to 1000" in {
        arabicValueOf('M') should be(1000)
    }

    private def arabicValueOf(digit: Char): Int = {
        new RomanDigit(digit).toArabic
    }
}