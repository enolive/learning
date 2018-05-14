import io.vavr.collection.List;

public class RomanConverter {
    private final List<Rule> rules = List.of(
            new Rule(1000, "M"),
            new Rule(900, "CM"),
            new Rule(500, "D"),
            new Rule(400, "CD"),
            new Rule(100, "C"),
            new Rule(90, "XC"),
            new Rule(50, "L"),
            new Rule(40, "XL"),
            new Rule(10, "X"),
            new Rule(9, "IX"),
            new Rule(5, "V"),
            new Rule(4, "IV"),
            new Rule(1, "I")
    );

    public String toRoman(int arabic) {
        return rules
                .foldLeft(
                        new ArabicToRomanConversion(arabic),
                        ArabicToRomanConversion::apply)
                .getResult();
    }

    public int toArabic(String input) {
        var romanToArabicConversion = new RomanToArabicConversion(input);
        romanToArabicConversion = romanToArabicConversion.apply("X", 10);
        romanToArabicConversion = romanToArabicConversion.apply("V", 5);
        romanToArabicConversion = romanToArabicConversion.apply("I", 1);
        return romanToArabicConversion.getResult();
    }
}
