class Roman(private val input: String) {
    fun toArabic(): Int {
        var sum = 0
        var remainingInput = input
        while (remainingInput.startsWith("I")) {
            sum += 1
            if (!remainingInput.isEmpty()) {
                remainingInput = remainingInput.substring(1)
            }
        }
        return sum
    }

}