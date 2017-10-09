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
            if (!conversion.remainingInput.startsWith(roman)) {
                return conversion
            }
            val newConversion = Conversion(conversion.remainingInput.cutAtStart(), conversion.sum + arabic)
            return applyRec(newConversion, roman, arabic)
        }
        return applyRec(this, roman, arabic)
    }


    private fun String.cutAtStart(): String {
        return when {
            !this.isEmpty() -> this.substring(1)
            else -> this
        }
    }
}
