import org.scalatest.{FlatSpec, Matchers}

class ConvertToArabicTest extends FlatSpec with Matchers {
    behavior of "Converting roman digits to arabic"

    it should "convert I to 1" in {
        RomanDigit('I').toArabic should be(1)
    }

    it should "convert V to 5" in {
        RomanDigit('V').toArabic should be(5)
    }

    it should "convert X to 10" in {
        RomanDigit('X').toArabic should be(10)
    }

    it should "convert L to 50" in {
        RomanDigit('L').toArabic should be(50)
    }

    it should "convert C to 100" in {
        RomanDigit('C').toArabic should be(100)
    }

    it should "convert D to 500" in {
        RomanDigit('D').toArabic should be(500)
    }

    it should "convert M to 1000" in {
        RomanDigit('M').toArabic should be(1000)
    }
}