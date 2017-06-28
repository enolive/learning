class Calculator {
    private val rules = sequenceOf(
            Rule(15, "Fizz-Buzz"),
            Rule(5, "Buzz"),
            Rule(3, "Fizz")
    )

    fun next(input: Int): String {
        return rules
                .filter { rule -> rule.appliesTo(input) }
                .map { rule -> rule.result }
                .firstOrNull() ?: input.toString()
    }
}

