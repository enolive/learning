class Conversion(private val remainingInput: Int, private val digits: Array<String> = emptyArray()) {
    val result get() = digits.joinToString("")

    fun apply(rule: Rule): Conversion {
        val howManyVs = remainingInput / rule.arabic
        val newDigits = digits + Array(howManyVs) { rule.roman }
        val newRemainingInput = remainingInput % rule.arabic
        return Conversion(newRemainingInput, newDigits)
    }
}