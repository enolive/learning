"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class FizzBuzz {
    constructor() {
        this.calculate = (input) => this.rules
            .filter(r => r.appliesTo(input))
            .map(r => r.result)
            .join('-') || input.toString();
        this.isDivisibleBy = (divisor) => (input) => input % divisor === 0;
        this.rules = [
            { appliesTo: this.isDivisibleBy(3), result: 'Fizz' },
            { appliesTo: this.isDivisibleBy(5), result: 'Buzz' },
        ];
    }
}
exports.FizzBuzz = FizzBuzz;
//# sourceMappingURL=fizz-buzz.js.map