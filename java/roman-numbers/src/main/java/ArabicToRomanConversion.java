import io.vavr.collection.Stream;

class ArabicToRomanConversion {
    private int remainingInput;
    private Stream<String> digits;

    ArabicToRomanConversion(int remainingInput) {
        this(remainingInput, Stream.empty());
    }

    private ArabicToRomanConversion(int remainingInput, Stream<String> digits) {
        this.remainingInput = remainingInput;
        this.digits = digits;
    }

    String getResult() {
        return String.join("", digits);
    }

    ArabicToRomanConversion apply(int arabic, String roman) {
        final var numberOfDigits = remainingInput / arabic;
        final var digits = this.digits.appendAll(repeat(numberOfDigits, roman));
        final var remainingInput = this.remainingInput % arabic;
        return new ArabicToRomanConversion(remainingInput, digits);
    }

    private Stream<String> repeat(int times, String roman) {
        return Stream.range(0, times).map(i -> roman);
    }
}
