internal class Conversion(private val remainingInput: String, val sum: Int = 0) {
    fun apply(rule: Rule): Conversion {
        val (roman, arabic) = rule
        tailrec fun applyRec(remainingInput: String, sum: Int): Conversion = when {
            !remainingInput.startsWith(roman) -> Conversion(remainingInput, sum)
            else -> applyRec(remainingInput.cutAtStart(roman), sum + arabic)
        }
        return applyRec(remainingInput, sum)
    }

    private fun String.cutAtStart(roman: String): String = when {
        this.length >= roman.length -> this.substring(roman.length)
        else -> this
    }

    fun failIfInputRemains(): Conversion {
        assert(remainingInput.isEmpty()) { 
            """expected that the input that remains is empty, but it doesn't.
                |this might be due to invalid input.: "$remainingInput"""".trimMargin() 
        }
        return this
    }
}