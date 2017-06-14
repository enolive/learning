import {Result} from "./Result";

export class Convert {
    public static toRomanNumber(arabic: number): string {
        const res = new Result(arabic);

        this.convertRomanDigit(res, 10, "X");
        this.convertRomanDigit(res, 9, "IX");
        this.convertRomanDigit(res, 5, "V");
        this.convertRomanDigit(res, 4, "IV");
        this.convertRomanDigit(res, 1, "I");
        return res.roman;
    }

    private static convertRomanDigit(res: Result, arabicDigit: number, romanDigit: string) {
        while (res.arabic >= arabicDigit) {
            res.roman += romanDigit;
            res.arabic -= arabicDigit;
        }
    }
}
