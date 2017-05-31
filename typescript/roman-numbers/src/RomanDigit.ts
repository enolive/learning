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

    private static convert(digit: string) {
        return RomanDigit.digitMap[digit];
    }

    private _first: string;

    constructor(private roman: string) {
        this._first = roman[0];
        RomanDigit.throwOnInvalidCharacter(this._first);
    }

    public toArabic(): number {
        const current = RomanDigit.convert(this._first);
        return this.isFollowingHigher()
            ? -current
            : current;
    }

    private isFollowingHigher(): boolean {
        const follower = this.roman[1];
        return RomanDigit.convert(follower) > RomanDigit.convert(this._first);
    }
}
