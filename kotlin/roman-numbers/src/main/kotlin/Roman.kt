class Roman(private val input: String) {
    fun toArabic(): Int {
        var sum = 0
        var remainingInput = input
        val pair = apply(remainingInput, sum, "V", 5)
        remainingInput = pair.first
        sum = pair.second
        val pair2 = apply(remainingInput, sum, "I", 1)
        return pair2.second
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
