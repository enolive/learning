"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fizz_buzz_1 = require("../src/fizz-buzz");
const chai_1 = require("chai");
describe('Fizz-Buzz', () => {
    let generator;
    beforeEach(() => generator = new fizz_buzz_1.FizzBuzz());
    it('should return normal numbers as is', () => {
        chai_1.expect(generator.calculate(1)).to.equal('1');
        chai_1.expect(generator.calculate(2)).to.equal('2');
    });
    it('should return numbers divisible by 3 as Fizz', () => {
        chai_1.expect(generator.calculate(3)).to.equal('Fizz');
        chai_1.expect(generator.calculate(9)).to.equal('Fizz');
    });
    it('should return numbers divisible by 5 as Buzz', () => {
        chai_1.expect(generator.calculate(5)).to.equal('Buzz');
        chai_1.expect(generator.calculate(10)).to.equal('Buzz');
    });
    it('should return numbers divisible by 15 as Fizz-Buzz', () => {
        chai_1.expect(generator.calculate(15)).to.equal('Fizz-Buzz');
        chai_1.expect(generator.calculate(30)).to.equal('Fizz-Buzz');
    });
});
//# sourceMappingURL=fizz-buzz.spec.js.map