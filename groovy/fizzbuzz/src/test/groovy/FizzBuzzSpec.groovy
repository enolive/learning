import spock.lang.Specification

class FizzBuzzSpec extends Specification {
    def "normal numbers should be returned as is"() {
        expect:
        calculate(input) == expected
        where:
        input || expected
        1     || "1"
        2     || "2"
    }

    def "numbers divisible by 3 should be returned as Fizz"() {
        expect:
        calculate(input) == expected
        where:
        input || expected
        3     || "Fizz"
        6     || "Fizz"
    }

    def "numbers divisible by 5 should be returned as Buzz"() {
        expect:
        calculate(input) == expected
        where:
        input || expected
        5     || "Buzz"
    }

    def calculate(int input) {
        switch (input) {
            case {isDivisibleBy(it, 3)}: return "Fizz"
            case {isDivisibleBy(it, 5)}: return "Buzz"
            default: return input.toString()
        }
    }

    private static isDivisibleBy(int input, int divisor) {
        input % divisor == 0
    }
}
