import org.scalatest.{FlatSpec, Matchers}

class AllDigitsTest extends FlatSpec with Matchers {
    behavior of "Roman digits"

    it should "have expected first digit" in {
        val digits = AllDigits.of("X")
        digits should contain only romanDigit('X')
    }

    it should "have expected three digits" in {
        val digits = AllDigits.of("XII")
        digits should contain allElementsOf List(romanDigit('X'), romanDigit('I'), romanDigit('I'))
    }

    private def romanDigit(digit: Char) = {
        new RomanDigit(digit)
    }
}
