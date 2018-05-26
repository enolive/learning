import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.List;

class RomanConverter {
    private final List<Tuple2<Integer, String>> rules = List.of(
            Tuple.of(1000, "M"),
            Tuple.of(900, "CM"),
            Tuple.of(500, "D"),
            Tuple.of(400, "CD"),
            Tuple.of(100, "C"),
            Tuple.of(90, "XC"),
            Tuple.of(50, "L"),
            Tuple.of(40, "XL"),
            Tuple.of(10, "X"),
            Tuple.of(9, "IX"),
            Tuple.of(5, "V"),
            Tuple.of(4, "IV"),
            Tuple.of(1, "I")
    );

    String toRoman(int arabic) {
        return rules
                .foldLeft(
                        new ArabicToRomanConversion(arabic),
                        ArabicToRomanConversion::apply)
                .getResult();
    }

    int toArabic(String input) {
        return rules
                .foldLeft(
                        new RomanToArabicConversion(input),
                        RomanToArabicConversion::apply)
                .getResult();

    }
}
