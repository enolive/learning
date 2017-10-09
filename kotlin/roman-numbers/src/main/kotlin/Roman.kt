class Roman(private val input: String) {
    fun toArabic(): Int {
        var sum = 0
        var remainingInput = input
        var conversion = Conversion(sum, remainingInput)
        conversion = apply(conversion, "V", 5)
        conversion = apply(conversion, "I", 1)
        return conversion.sum
    }

    private fun apply(remainingInput: String, sum: Int, roman: String, arabic: Int): Pair<String, Int> {
        var conversion = Conversion(sum, remainingInput)
        conversion = apply(conversion, roman, arabic)
        return Pair(conversion.remainingInput, conversion.sum)
    }

    private fun apply(conversion: Conversion, roman: String, arabic: Int): Conversion {
        var newRemainingInput = conversion.remainingInput
        var newSum = conversion.sum
        while (newRemainingInput.startsWith(roman)) {
            newSum += arabic
            newRemainingInput = newRemainingInput.cutAtStart()
        }
        return Conversion(newSum, newRemainingInput)
    }

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }

}

class Conversion(val sum: Int, val remainingInput: String) {

}
