internal class Conversion(private val remainingInput: String, val sum: Int = 0) {
    fun apply(rule: Rule): Conversion {
        tailrec fun applyRec(remainingInput: String, sum: Int): Conversion {
            return when {
                !remainingInput.startsWith(rule.roman) -> Conversion(remainingInput, sum)
                else -> applyRec(remainingInput.cutAtStart(rule.roman), sum + rule.arabic)
            }
        }
        return applyRec(remainingInput, sum)
    }

    private fun String.cutAtStart(roman: String): String = when {
        this.length >= roman.length -> this.substring(roman.length)
        else -> this
    }

    fun failIfInputRemains() {
        assert(remainingInput.isEmpty()) { 
            """expected that the input that remains is empty, but it doesn't.
                |$remainingInput""".trimMargin() 
        }
    }
}