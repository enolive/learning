export class FizzBuzzGenerator {
    private rules = [
        {appliesTo: this.numberDivisibleBy(3), result: 'Fizz'},
        {appliesTo: this.numberDivisibleBy(5), result: 'Buzz'},
        {appliesTo: this.contains(27), result: 'Porn'},
        {appliesTo: this.numberDivisibleBy(7), result: 'Zazz'},
    ]

    generate(input: number) {
        return this.rules
            .filter(r => r.appliesTo(input))
            .map(r => r.result)
            .join('-') || input.toString()
    }

    private numberDivisibleBy(denominator: number) {
        return (input: number) =>
            input !== 0 && input % denominator === 0
    }

    private contains(infix: number) {
        return (input: number) =>
            input.toString().includes(infix.toString())
    }
}
