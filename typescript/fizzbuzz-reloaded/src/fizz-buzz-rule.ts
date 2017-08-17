import {IRule} from "./irule";

export class FizzBuzzRule implements IRule {
    private divisors: number[];
    private result: string;

    constructor(result: string, ...divisors: number[]) {
        this.result = result;
        this.divisors = divisors;
    }

    appliesTo(input: number) {
        return this.divisors.every(d => input % d === 0);
    }

    // noinspection JSUnusedLocalSymbols
    resultFor(input: number) {
        return this.result;
    }
}
