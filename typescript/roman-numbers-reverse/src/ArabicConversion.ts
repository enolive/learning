import {Rule} from "./Rule";
export class ArabicConversion {
    public get result(): string {
        return this._result;
    }

    private _result: string;
    private arabic: number;

    constructor(arabicNumber: number) {
        this.arabic = arabicNumber;
        this._result = "";
    }

    public apply(rule: Rule) {
        while (this.arabic >= rule.arabic) {
            this._result += rule.roman;
            this.arabic -= rule.arabic;
        }
    }
}
