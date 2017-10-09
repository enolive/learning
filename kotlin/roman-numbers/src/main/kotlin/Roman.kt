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
        tailrec fun applyRec(conversion: Conversion, roman: String, arabic: Int): Conversion {
            val (remainingInput, sum) = conversion
            if (!remainingInput.startsWith(roman)) {
                return conversion
            }
            val newConversion = Conversion(remainingInput.cutAtStart(), sum + arabic)
            return applyRec(newConversion, roman, arabic)
        }
        return applyRec(this, roman, arabic)
    }

    private operator fun component1() = remainingInput

    private operator fun component2() = sum

    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }
}
