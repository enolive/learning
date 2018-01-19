export class FizzBuzz {
    private rules = [
        {appliesTo: this.isDivisibleBy(3), result: 'Fizz'},
        {appliesTo: this.isDivisibleBy(5), result: 'Buzz'},
    ]

    calculate(input: number): string {
        return this.rules
            .filter(r => r.appliesTo(input))
            .map(r => r.result)
            .join('-') || input.toString()
    }

    private isDivisibleBy(divisor: number) {
        return (input: number) =>
            input % divisor === 0
    }
}
