import {ArabicConversion} from "./ArabicConversion";

export class Convert {
    public static toRomanNumber(arabic: number): string {
        const conversion = new ArabicConversion(arabic);

        conversion.convertRomanDigit("X", 10);
        conversion.convertRomanDigit("IX", 9);
        conversion.convertRomanDigit("V", 5);
        conversion.convertRomanDigit("IV", 4);
        conversion.convertRomanDigit("I", 1);
        return conversion.roman;
    }
}
