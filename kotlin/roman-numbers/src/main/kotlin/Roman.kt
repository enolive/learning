class Roman(private val input: String) {
    fun toArabic(): Int {
        var sum = 0
        var remainingInput = input
        while (remainingInput.startsWith("V")) {
            sum += 5
            remainingInput = remainingInput.cutAtStart()
        }
        while (remainingInput.startsWith("I")) {
            sum += 1
            remainingInput = remainingInput.cutAtStart()
        }
        return sum
    }

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }

}