internal class Conversion(private val remainingInput: String, val sum: Int = 0) {
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