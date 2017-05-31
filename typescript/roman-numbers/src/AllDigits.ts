import {RomanDigit} from "./RomanDigit";

export class AllDigits {
    public static of(roman: string) {
        return roman
            .split("")
            .map((c, i) => new RomanDigit(roman.substr(i)));
    }
}
