export class ArabicConversion {
    public roman: string;
    public arabic: number;

    constructor(arabicNumber: number) {
        this.arabic = arabicNumber;
        this.roman = "";
    }

    public convertRomanDigit(romanDigit: string, arabicDigit: number) {
        while (this.arabic >= arabicDigit) {
            this.roman += romanDigit;
            this.arabic -= arabicDigit;
        }
    }

}
