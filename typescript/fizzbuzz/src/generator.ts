let isDivisibleBy = function (number: number, denominator: number) {
    return number % denominator == 0;
};

export class Generator {
    resultFor(number: number): string {
        if (isDivisibleBy(number, 3)) {
            return 'Fizz';
        }

        return number.toString();
    }
}
