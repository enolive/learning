
object AllDigits {
    def of(roman: String): Iterable[RomanDigit] = {
        for (r <- roman) yield new RomanDigit(r)
    }
}
