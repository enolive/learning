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

    public convertRomanDigit(romanDigit: string, arabicDigit: number) {
        while (this.arabic >= arabicDigit) {
            this._result += romanDigit;
            this.arabic -= arabicDigit;
        }
    }

}
