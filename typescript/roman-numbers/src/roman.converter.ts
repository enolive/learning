export class RomanConverter {

    public toArabic(roman: string): number {
        const character = roman[0];
        //noinspection TsLint
        const digits = {
            I: 1,
            V: 5,
            X: 10,
            L: 50,
        };

        return digits[character];
    }
}
