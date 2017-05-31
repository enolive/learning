import {AllDigits} from "./all.digits";
import {NumberCollection} from "./number.collection";

export class ConvertRoman {
    public static toArabic(roman: string): number {
        return AllDigits.of(roman)
            .map((d) => d.toArabic())
            .reduce(NumberCollection.sum);
    }
}
