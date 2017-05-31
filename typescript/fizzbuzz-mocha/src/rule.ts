export class Rule {
    get result(): string {
        return this._result;
    }

    private _result: string;
    private _denominators: number[];

    constructor(result: string, denominators: number[]) {
        this._result = result;
        this._denominators = denominators;
    }

    public appliesTo(input: number): boolean {
        return this._denominators.every((d) => input % d === 0);
    }
}
