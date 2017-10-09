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
        var remainingInput1 = remainingInput
        var sum1 = sum
        while (remainingInput1.startsWith(roman)) {
            sum1 += arabic
            remainingInput1 = remainingInput1.cutAtStart()
        }
        return Pair(remainingInput1, sum1)
    }

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }

}