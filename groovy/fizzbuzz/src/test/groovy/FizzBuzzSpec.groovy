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
        10    || "Buzz"
    }

    def "numbers divisible by 3 and 5 should be returned as Fizz-Buzz"() {
        expect:
        calculate(input) == expected
        where:
        input || expected
        15    || "Fizz-Buzz"
    }

    def calculate(int input) {
        def rules = [
                new Tuple2<Integer, String>(3, "Fizz"),
                new Tuple2<Integer, String>(5, "Buzz"),
        ]
        def result = rules.findAll { it -> isDivisibleBy(input, it.first) }
                .collect { it -> it.second }
                .join('-')
        if (result.empty) input.toString() else result
    }

    private static isDivisibleBy(int input, int divisor) {
        input % divisor == 0
    }
}
