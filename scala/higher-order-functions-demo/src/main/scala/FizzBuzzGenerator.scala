import Extensions.Division

class FizzBuzzGenerator() {
    val rules = Array(
        (divisibleBy(15), fixedValue("Fizz-Buzz")),
        (divisibleBy(3), fixedValue("Fizz")),
        (divisibleBy(5), fixedValue("Buzz")),
        (other, returnInput)
    )
    
    def nextNumber(input: Int): String = {
        val rulesThatMatch = rules
            .filter { case (doesTheRuleApply, _) => doesTheRuleApply(input) }
        val transformedResults = rulesThatMatch
            .map { case (_, result) => result(input) }
        transformedResults.head
    }

    private def returnInput = (i: Int) => i.toString

    private def fixedValue(result: String): (Int) => String = (_: Int) => result

    private def other = (_: Int) => true

    private def divisibleBy(divisor: Int) = (i: Int) => i isDivisibleBy divisor
}
