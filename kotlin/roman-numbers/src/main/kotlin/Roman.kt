class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        conversion = conversion.apply("V", 5)
        conversion = conversion.apply("I", 1)
        return conversion.sum
    }

}

class Conversion(private val remainingInput: String, val sum: Int = 0) {
    fun apply(roman: String, arabic: Int): Conversion {
        tailrec fun applyRec(remainingInput: String, sum: Int): Conversion {
            return when {
                !remainingInput.startsWith(roman) -> Conversion(remainingInput, sum)
                else -> applyRec(remainingInput.cutAtStart(), sum + arabic)
            }
        }
        return applyRec(remainingInput, sum)
    }

    private fun String.cutAtStart(): String = when {
        !this.isEmpty() -> this.substring(1)
        else -> this
    }
}
