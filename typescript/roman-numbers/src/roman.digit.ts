export class RomanDigit {
    private _roman: string;

    //noinspection TsLint - ordering is not appropriate here
    private static digitMap = {
        I: 1,
        V: 5,
        X: 10,
        L: 50,
        C: 100,
        D: 500,
        M: 1000,
    };

    constructor(roman: string) {
        if (!(roman in RomanDigit.digitMap)) {
            throw RangeError(`Invalid roman digit '${roman}'.`);
        }
        this._roman = roman;
    }

    public toArabic(): number {
        return RomanDigit.digitMap[this._roman];
    }
}
