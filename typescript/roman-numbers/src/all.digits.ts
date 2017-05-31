import {RomanDigit} from "./roman.digit";

export class AllDigits {
    public static of(roman: string) {
        return roman.split("").map((c) => new RomanDigit(c));
    }
}
