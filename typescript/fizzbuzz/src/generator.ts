export class Generator {
    resultFor(number: number): string {
        if (number == 3) {
            return 'Fizz';
        }

        return number.toString();
    }
}
