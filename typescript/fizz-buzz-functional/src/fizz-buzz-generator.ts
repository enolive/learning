export class FizzBuzzGenerator {
    generate(input: number) {
        if (input % 3 === 0) {
            return 'Fizz'
        }

        return input.toString()
    }
}