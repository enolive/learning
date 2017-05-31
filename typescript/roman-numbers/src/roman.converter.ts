export class RomanConverter {

    private static count(a: number, b: number): number {
        return a + b;
    }

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
        return roman
            .split("")
            .map((c) => this.convertDigit(c))
            .reduce(RomanConverter.count);
    }

    private convertDigit(character: string): number {
        if (!(character in this.digitMap)) {
            throw RangeError(`Invalid roman digit '${character}'.`);
        }
        return this.digitMap[character];
    }
}
