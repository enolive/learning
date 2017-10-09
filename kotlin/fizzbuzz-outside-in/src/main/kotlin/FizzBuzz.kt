class FizzBuzz(private vararg val rules: Rule = arrayOf(
        ConcreteRule(isDivisibleBy(3), "Fizz"),
        ConcreteRule(isDivisibleBy(5), "Buzz")
)) {
    fun calculate(input: Int): String = rules
            .filter { it.appliesTo(input) }
            .joinToString("-") { it.result }
            .orWhenEmpty { input.toString() }

    private fun String.orWhenEmpty(alternative: () -> String): String = when {
        isEmpty() -> alternative()
        else -> this
    }
}

private fun isDivisibleBy(d: Int) = { input: Int -> input % d == 0 }

private class ConcreteRule(val applies: (Int) -> Boolean, override val result: String) : Rule {
    override fun appliesTo(input: Int): Boolean = applies(input)
}
