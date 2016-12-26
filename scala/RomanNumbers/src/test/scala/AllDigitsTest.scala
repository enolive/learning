import org.scalatest.{FlatSpec, Matchers}

class AllDigitsTest extends FlatSpec with Matchers {
    behavior of "Roman digits"

    it should "have expected first digit" in {
        val digits = AllDigits.of("X")
        digits should contain only new RomanDigit('X')
    }

    it should "have expected three digits" in {
        val digits = AllDigits.of("XII")
        digits should contain allElementsOf List(new RomanDigit('X'), new RomanDigit('I'), new RomanDigit('I'))
    }
}
