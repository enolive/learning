"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fizz_buzz_control_1 = require("../src/fizz-buzz-control");
const chai_1 = require("chai");
describe('Fizz-Buzz Control', () => {
    let control;
    beforeEach(() => control = new fizz_buzz_control_1.FizzBuzzControl());
    it('should throw on invalid limit', () => {
        // noinspection TypeScriptValidateTypes
        chai_1.expect(() => control.calculateUpTo(NaN).subscribe()).to.throw(RangeError);
        chai_1.expect(() => control.calculateUpTo(1001).subscribe()).to.throw(RangeError);
    });
    it('should return the expected results', () => {
        // noinspection TypeScriptValidateTypes
        control.calculateUpTo(15).subscribe(result => chai_1.expect(result).to.deep.equal([
            '1',
            '2',
            'Fizz',
            '4',
            'Buzz',
            'Fizz',
            '7',
            '8',
            'Fizz',
            'Buzz',
            '11',
            'Fizz',
            '13',
            '14',
            'Fizz-Buzz',
        ]));
    });
});
//# sourceMappingURL=fizz-buzz-control.spec.js.map