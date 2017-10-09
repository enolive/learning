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
        if (!remainingInput.startsWith(roman)) {
            return this
        }
        val newConversion = Conversion(remainingInput.cutAtStart(), sum + arabic)
        return newConversion.apply(roman, arabic)
    }

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }
}
