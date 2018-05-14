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
        var result = 0;
        var romanToArabicConversion = new RomanToArabicConversion(input, result);
        romanToArabicConversion = romanToArabicConversion.apply("V", 5);
        input = romanToArabicConversion.getInput();
        result = romanToArabicConversion.getResult();
        while (input.startsWith("I")) {
            result = result + 1;
            input = input.substring("I".length());
        }
        return result;

    }

    private class RomanToArabicConversion {
        private String input;
        private int result;

        public RomanToArabicConversion(String input, int result) {
            this.input = input;
            this.result = result;
        }

        public String getInput() {
            return input;
        }

        public int getResult() {
            return result;
        }

        public RomanToArabicConversion apply(String roman, int arabic) {
            var newResult = result;
            var newInput = input;
            while (newInput.startsWith(roman)) {
                newResult = newResult + arabic;
                newInput = newInput.substring(roman.length());
            }
            return new RomanToArabicConversion(newInput, newResult);
        }
    }
}
