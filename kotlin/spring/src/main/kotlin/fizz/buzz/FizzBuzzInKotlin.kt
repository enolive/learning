package fizz.buzz

import org.springframework.stereotype.Service

@Service
class FizzBuzzInKotlin : FizzBuzzService {
    private val rules = arrayOf(
            Rule(isDivisibleBy(3), "Fizz"),
            Rule(isDivisibleBy(5), "Buzz"),
            Rule(isDivisibleBy(7), "Zazz")
    )

    override fun calculateUpTo(limit: Int) =
        (1..limit)
            .map { calculate(it) }
            .toTypedArray()

    private fun isDivisibleBy(divisor: Int) =
        { number: Int -> number % divisor == 0 }

    private data class Rule(val appliesTo: (Int) -> Boolean, val result: String)

    fun calculate(number: Int) =
        rules
            .filter { it.appliesTo(number) }
            .joinToString("-") { it.result }
            .orIfEmpty { number.toString() }

    private fun String.orIfEmpty(alternative: () -> String) =
        if (isEmpty()) alternative() else this
}