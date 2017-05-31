import {AllDigits} from "./AllDigits";
import {NumberCollection} from "./NumberCollection";

export class ConvertRoman {
    public static toArabic(roman: string): number {
        return AllDigits.of(roman)
            .map((d) => d.toArabic())
            .reduce(NumberCollection.sum);
    }
}
