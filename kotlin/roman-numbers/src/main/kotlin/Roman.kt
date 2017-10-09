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
        val conversion = this
        var newRemainingInput = conversion.remainingInput
        var newSum = conversion.sum
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
