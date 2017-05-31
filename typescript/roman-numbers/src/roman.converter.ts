import {NumberCollection} from "./number.collection";

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
        return roman
            .split("")
            .map((c) => this.convertDigit(c))
            .reduce(NumberCollection.sum);
    }

    private convertDigit(character: string): number {
        if (!(character in this.digitMap)) {
            throw RangeError(`Invalid roman digit '${character}'.`);
        }
        return this.digitMap[character];
    }
}
