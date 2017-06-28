class Calculator {
    fun next(input: Int): String {
        when {
            isDivisibleBy(input, 15) -> return "Fizz-Buzz"
            isDivisibleBy(input, 3) -> return "Fizz"
            isDivisibleBy(input, 5) -> return "Buzz"
            else -> return input.toString()
        }

    }

    private fun isDivisibleBy(input: Int, denominator: Int) = input % denominator == 0
}