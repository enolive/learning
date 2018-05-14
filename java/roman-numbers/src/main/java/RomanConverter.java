import io.vavr.collection.List;

public class RomanConverter {
    public String toRoman(int arabic) {
        var rules = List.of(
                new Rule(10, "X"),
                new Rule(9, "IX"),
                new Rule(5, "V"),
                new Rule(4, "IV"),
                new Rule(1, "I")
        );
        return rules
                .foldLeft(
                        new ArabicToRomanConversion(arabic),
                        ArabicToRomanConversion::apply)
                .getResult();
    }

}
