export class RomanConverter {

    //noinspection TsLint - ordering is not appropriate here
    private digitMap = {
        I: 1,
        V: 5,
        X: 10,
        L: 50,
        C: 100,
        D: 500,
        M: 1000,
    };

    public toArabic(roman: string): number {
        const character = roman[0];
        return this.convertDigit(character);
    }

    private convertDigit(character: string): number {
        const digit = this.digitMap[character];
        if (!digit) {
            throw RangeError(`Invalid roman digit '${character}'.`);
        }
        return digit;
    }
}
