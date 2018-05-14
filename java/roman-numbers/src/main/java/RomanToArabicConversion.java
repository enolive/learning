class RomanToArabicConversion {
    private String input;
    private int result;

    RomanToArabicConversion(String input) {
        this(input, 0);
    }

    private RomanToArabicConversion(String input, int result) {
        this.input = input;
        this.result = result;
    }

    int getResult() {
        return result;
    }

    RomanToArabicConversion apply(Rule rule) {
        var newResult = result;
        var newInput = input;
        while (newInput.startsWith(rule.getRoman())) {
            newResult = newResult + rule.getArabic();
            newInput = newInput.substring(rule.getRoman().length());
        }
        return new RomanToArabicConversion(newInput, newResult);
    }
}
