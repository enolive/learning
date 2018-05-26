import io.vavr.Function2;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.List;

class RomanNumbersConverter {

    private final List<Tuple2<Integer, String>> digits = List.of(
            Tuple.of(1, "I"),
            Tuple.of(4, "IV"),
            Tuple.of(5, "V"),
            Tuple.of(9, "IX"),
            Tuple.of(10, "X"),
            Tuple.of(40, "XL"),
            Tuple.of(50, "L"),
            Tuple.of(90, "XC"),
            Tuple.of(100, "C"),
            Tuple.of(400, "CD"),
            Tuple.of(500, "D"),
            Tuple.of(900, "CM"),
            Tuple.of(1000, "M")
    );

    String toRoman(int arabic) {
        return digits
                .foldRight(
                        new ArabicToRomanConversion(arabic),
                        Function2.of(ArabicToRomanConversion::apply).reversed()
                )
                .getResult();
    }

    int toArabic(String roman) {
        return digits
                .foldRight(
                        new RomanToArabicConversion(roman),
                        (digit, acc) -> acc.apply(digit)
                )
                .getResult();


    }
}