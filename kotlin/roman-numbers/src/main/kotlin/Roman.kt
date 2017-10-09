class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        conversion = conversion.apply("V", 5)
        conversion = conversion.apply("I", 1)
        return conversion.sum
    }

}

class Conversion(val remainingInput: String, val sum: Int = 0) {
    fun apply(roman: String, arabic: Int): Conversion {
        var newRemainingInput = remainingInput
        var newSum = sum
        while (newRemainingInput.startsWith(roman)) {
            newSum += arabic
            newRemainingInput = newRemainingInput.cutAtStart()
        }
        return Conversion(newRemainingInput, newSum)
    }

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }
}
