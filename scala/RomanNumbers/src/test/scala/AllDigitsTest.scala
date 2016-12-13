import org.scalatest.{FlatSpec, Matchers}

class AllDigitsTest extends FlatSpec with Matchers {
    behavior of "Roman digits"

    it should "have expected first digit" in {
        val digits = AllDigits.of("X")
        digits should contain only RomanDigit('X')
    }

    it should "have expected three digits" in {
        val digits = AllDigits.of("XII")
        digits should contain allElementsOf List(RomanDigit('X'), RomanDigit('I'), RomanDigit('I'))
    }
}
