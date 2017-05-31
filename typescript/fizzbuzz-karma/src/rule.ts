export class Rule {
    get result(): string {
        return this._result;
    }

    private _denominator: number[];
    private _result: string;

    constructor(result: string, ...denominator: number[]) {
        this._denominator = denominator;
        this._result = result;
    }

    appliesTo(number: number): boolean {
        return this._denominator.every(d => number % d == 0);
    }
}
