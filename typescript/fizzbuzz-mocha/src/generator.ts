export class Generator {
    public resultFor(input: number): string {
        if (input === 3) {
            return "Fizz";
        }

        return input.toString();
    }
}
