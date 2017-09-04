export class FizzBuzzGenerator {
    generate(input: number) {
        if (this.isDivisibleBy(input, 15)) {
            return 'Fizz-Buzz'
        }
        if (this.isDivisibleBy(input, 5)) {
            return 'Buzz'
        }
        if (this.isDivisibleBy(input, 3)) {
            return 'Fizz'
        }

        return input.toString()
    }

    private isDivisibleBy(input: number, denominator: number) {
        return input !== 0 && input % denominator === 0
    }
}