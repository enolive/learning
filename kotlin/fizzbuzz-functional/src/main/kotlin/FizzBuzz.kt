import kotlin.coroutines.experimental.buildSequence

class Rule(val appliesTo: (Int) -> Boolean, val resultFor: (Int) -> String)

fun generate(input: Int): String = allRules()
        .filter { it.appliesTo(input) }
        .map { it.resultFor(input) }
        .first()

private fun allRules() = buildSequence {
    yield(isDivisibleBy(3, 5) to returns("Fizz-Buzz"))
    yield(isDivisibleBy(5) to returns("Buzz"))
    yield(isDivisibleBy(3) to returns("Fizz"))
    yield(otherwise to numberItself)
}.map { Rule(it.first, it.second) }

private val otherwise = { _: Int -> true }
private val numberItself = { input: Int -> input.toString() }
private val returns = { constantValue: String -> { _: Int -> constantValue } }

private fun isDivisibleBy(vararg denominators: Int) =
        { input: Int -> denominators.all { input % it == 0 } }
