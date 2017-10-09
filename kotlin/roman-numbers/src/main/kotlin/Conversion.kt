internal class Conversion(private val remainingInput: String, val sum: Int = 0) {
    fun apply(rule: Rule): Conversion {
        tailrec fun applyRec(remainingInput: String, sum: Int): Conversion {
            return when {
                !remainingInput.startsWith(rule.roman) -> Conversion(remainingInput, sum)
                else -> applyRec(remainingInput.cutAtStart(), sum + rule.arabic)
            }
        }
        return applyRec(remainingInput, sum)
    }

    private fun String.cutAtStart(): String = when {
        !this.isEmpty() -> this.substring(1)
        else -> this
    }
}