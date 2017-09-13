export class FizzBuzzGenerator {
    generate(input: number) {
        const resultSequence = this.allTheRules()
            .filter(r => r.appliesTo(input))
            .map(r => r.result)
        return resultSequence.length !== 0
            ? resultSequence.join('-')
            : input.toString()
    }

    private allTheRules(): Array<{ appliesTo: (input: number) => boolean, result: string }> {
        const numberDivisibleBy = denominator => input =>
            input !== 0 && input % denominator === 0
        const contains = infix => input =>
            input.toString().includes(infix)

        return [
            {appliesTo: numberDivisibleBy(3), result: 'Fizz'},
            {appliesTo: numberDivisibleBy(5), result: 'Buzz'},
            {appliesTo: contains(27), result: 'Porn'},
            {appliesTo: numberDivisibleBy(7), result: 'Zazz'},
        ]
    }
}
