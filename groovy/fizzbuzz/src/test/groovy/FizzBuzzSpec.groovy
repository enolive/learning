import spock.lang.Specification

import static FizzBuzz.calculate

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
        30    || "Fizz-Buzz"
    }
}
