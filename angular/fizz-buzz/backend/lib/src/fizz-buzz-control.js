"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fizz_buzz_1 = require("./fizz-buzz");
const Observable_1 = require("rxjs/Observable");
require("rxjs/add/observable/range");
require("rxjs/add/observable/throw");
require("rxjs/add/operator/map");
require("rxjs/add/operator/toArray");
class FizzBuzzControl {
    constructor() {
        this.calculateUpTo = (limit) => !this.isValid(limit)
            ? Observable_1.Observable.throw(new RangeError('invalid limit'))
            : Observable_1.Observable
                .range(1, limit)
                .map(n => this.generator.calculate(n))
                .toArray();
        this.generator = new fizz_buzz_1.FizzBuzz();
        this.isValid = (limit) => !isNaN(limit) && limit <= 1000;
    }
}
exports.FizzBuzzControl = FizzBuzzControl;
//# sourceMappingURL=fizz-buzz-control.js.map