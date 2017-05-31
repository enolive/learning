import {RomanDigit} from "./roman.digit";

export class AllDigits {
    public static of(roman: string) {
        return roman
            .split("")
            .map((c, i) => new RomanDigit(roman.substr(i)));
    }
}
