export class FizzBuzzGenerator {
    generate(input: number) {
        const rules = [
            {denominators: [3, 5], result: 'Fizz-Buzz'},
            {denominators: [5], result: 'Buzz'},
            {denominators: [3], result: 'Fizz'},
        ]

        const [match] = rules
            .filter(r => this.isDivisibleBy(input, r.denominators))
            .map(r => r.result)
        return match || input.toString()
    }

    private isDivisibleBy(input: number, denominators: number[]) {
        return denominators.every(denominator => input !== 0 && input % denominator === 0)
    }
}