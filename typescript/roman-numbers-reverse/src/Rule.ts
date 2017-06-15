export class Rule {
    private _roman: string;
    private _arabic: number;

    public get roman(): string {
        return this._roman;
    }

    public get arabic(): number {
        return this._arabic;
    }

    public constructor(parameters: { roman: string, arabic: number }) {
        const {roman, arabic} = parameters;
        this._roman = roman;
        this._arabic = arabic;
    }
}
