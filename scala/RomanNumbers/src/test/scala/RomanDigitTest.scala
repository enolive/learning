import org.scalatest.{FlatSpec, Matchers}

class RomanDigitTest extends FlatSpec with Matchers {
    behavior of "Converting roman digits to arabic"

    it should "convert I to 1" in {
        theArabicRepresentationOf('I') should be(1)
    }

    it should "convert V to 5" in {
        theArabicRepresentationOf('V') should be(5)
    }

    it should "convert X to 10" in {
        theArabicRepresentationOf('X') should be(10)
    }

    it should "convert L to 50" in {
        theArabicRepresentationOf('L') should be(50)
    }

    it should "convert C to 100" in {
        theArabicRepresentationOf('C') should be(100)
    }

    it should "convert D to 500" in {
        theArabicRepresentationOf('D') should be(500)
    }

    it should "convert M to 1000" in {
        theArabicRepresentationOf('M') should be(1000)
    }

    private def theArabicRepresentationOf(digit: Char): Int = {
        new RomanDigit(digit).toArabic
    }
}