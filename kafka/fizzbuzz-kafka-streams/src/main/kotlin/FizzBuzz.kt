import arrow.core.getOrElse
import arrow.core.toOption
import arrow.syntax.function.curried
import arrow.syntax.function.reverse

class FizzBuzz {
    private val rules = listOf(
            Rule(numbersDivisibleBy(3), "Fizz"),
            Rule(numbersDivisibleBy(5), "Buzz")
    )

    fun sequenceUpTo(limit: Int) = (1..limit).map(::single)

    fun single(input: Int) = tryGetResultFromRules(input)
            .toOption()
            .filter { it.isNotEmpty() }
            .getOrElse { input.toString() }

    private fun numbersDivisibleBy(divisor: Int): (input: Int) -> Boolean =
            ::isDivisibleBy
                    .reverse()
                    .curried()(divisor)

    private fun isDivisibleBy(input: Int, divisor: Int) = input % divisor == 0

    private fun tryGetResultFromRules(input: Int) = rules
            .asSequence()
            .filter { it.appliesTo(input) }
            .joinToString("-") { it.result }
}

private data class Rule(val appliesTo: (Int) -> Boolean, val result: String)