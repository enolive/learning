export class RomanDigit {
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

    private static throwOnInvalidCharacter(roman: string) {
        if (!(roman in RomanDigit.digitMap)) {
            throw RangeError(`Invalid roman digit '${roman}'.`);
        }
    }

    constructor(private roman: string) {
        RomanDigit.throwOnInvalidCharacter(roman);
    }

    public toArabic(): number {
        return RomanDigit.digitMap[this.roman];
    }
}
