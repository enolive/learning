import arrow.core.Option
import arrow.core.getOrElse

class FizzBuzz {
    private val rules = listOf(
            Rule(3, "Fizz"),
            Rule(5, "Buzz")
    )

    fun sequenceUpTo(limit: Int) = (1..limit).map(::single)

    fun single(input: Int) = Option.just(tryGetResultFromRules(input))
            .filter { it.isNotEmpty() }
            .getOrElse { input.toString() }

    private fun tryGetResultFromRules(input: Int) = rules
            .asSequence()
            .filter { it.appliesTo(input) }
            .joinToString("-") { it.result }
}

data class Rule(private val divisor: Int, val result: String) {
    fun appliesTo(input: Int) = input % divisor == 0
}