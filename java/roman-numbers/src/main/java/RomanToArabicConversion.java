class RomanToArabicConversion {
    private String input;
    private int result;

    public RomanToArabicConversion(String input) {
        this(input, 0);
    }

    public RomanToArabicConversion(String input, int result) {
        this.input = input;
        this.result = result;
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
