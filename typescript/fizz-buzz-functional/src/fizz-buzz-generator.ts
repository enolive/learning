export class FizzBuzzGenerator {
    generate(input: number) {
        function isDivisibleBy(i: number, denominator: number) {
            return i !== 0 && i % denominator === 0
        }

        if (isDivisibleBy(input, 3) && isDivisibleBy(input, 5)) {
            return 'Fizz-Buzz'
        }
        if (isDivisibleBy(input, 5)) {
            return 'Buzz'
        }
        if (isDivisibleBy(input, 3)) {
            return 'Fizz'
        }

        return input.toString()
    }
}
