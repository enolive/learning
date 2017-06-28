class Calculator {

    fun next(input: Int): String {
        val rules = sequenceOf(
                ConcreteRule(15, "Fizz-Buzz"),
                ConcreteRule(5, "Buzz"),
                ConcreteRule(3, "Fizz"),
                DefaultRule(input)
        )

        return rules
                .filter { rule -> rule.appliesTo(input) }
                .map { rule -> rule.result }
                .first()
    }
}

