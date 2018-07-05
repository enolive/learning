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

    def calculate(int input) {
        input.toString()
    }
}
