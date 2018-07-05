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

    def calculate(int input) {
        if (input % 3 == 0) {
            return "Fizz"
        }
        input.toString()
    }
}
