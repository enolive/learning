package hello

import org.springframework.stereotype.Service

@Service
class FizzBuzzInKotlin : FizzBuzzService {
    private val rules = arrayOf(
            Rule(isDivisibleBy(3), "Fizz"),
            Rule(isDivisibleBy(5), "Buzz")
    )

    override fun calculateUpTo(limit: Int): Array<String> {
        return generateSequence(1) { number -> number + 1 }
                .map { calculate(it) }
                .take(limit)
                .toList()
                .toTypedArray()
    }

    private fun isDivisibleBy(divisor: Int): (Int) -> Boolean {
        return { number: Int -> number % divisor == 0 }
    }

    private data class Rule(val appliesTo: (Int) -> Boolean, val result: String)

    private fun calculate(number: Int): String {
        return rules
                .filter { it.appliesTo(number) }
                .joinToString("-") { it.result }
                .orIfEmpty { number.toString() }
    }

    private fun String.orIfEmpty(alternative: () -> String): String {
        return if (isEmpty()) alternative() else this
    }
}