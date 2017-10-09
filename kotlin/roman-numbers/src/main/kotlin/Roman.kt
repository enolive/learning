class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        conversion = apply(conversion, "V", 5)
        conversion = apply(conversion, "I", 1)
        return conversion.sum
    }

    private fun apply(conversion: Conversion, roman: String, arabic: Int): Conversion {
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

class Conversion(val remainingInput: String, val sum: Int = 0) {

}
