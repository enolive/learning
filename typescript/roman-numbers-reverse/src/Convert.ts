import {ArabicConversion} from "./ArabicConversion";
import {Rule} from "./Rule";

export class Convert {
    public static toRomanNumber(arabic: number): string {
        const conversion = new ArabicConversion(arabic);
        Convert.chainOfRules.forEach((r) => conversion.apply(r));
        return conversion.result;
    }

    private static chainOfRules = [
        new Rule({roman: "M", arabic: 1000}),
        new Rule({roman: "CM", arabic: 900}),
        new Rule({roman: "D", arabic: 500}),
        new Rule({roman: "CD", arabic: 400}),
        new Rule({roman: "C", arabic: 100}),
        new Rule({roman: "XC", arabic: 90}),
        new Rule({roman: "L", arabic: 50}),
        new Rule({roman: "XL", arabic: 40}),
        new Rule({roman: "X", arabic: 10}),
        new Rule({roman: "IX", arabic: 9}),
        new Rule({roman: "V", arabic: 5}),
        new Rule({roman: "IV", arabic: 4}),
        new Rule({roman: "I", arabic: 1}),
    ];
}
