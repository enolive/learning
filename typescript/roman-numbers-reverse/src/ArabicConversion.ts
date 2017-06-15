import {Rule} from "./Rule";
export class ArabicConversion {
    public get result(): string {
        return this._result;
    }

    private _result: string;
    private arabic: number;

    constructor(arabic: number) {
        this.arabic = arabic;
        this._result = "";
    }

    public apply(rule: Rule) {
        if (this.arabic < rule.arabic) {
            return;
        }

        this._result += rule.roman;
        this.arabic -= rule.arabic;
        return this.apply(rule);
    }
}
