import io.vavr.Tuple2;
import io.vavr.collection.Stream;

class ArabicToRomanConversion {
    private final int input;
    private final Stream<String> digits;

    ArabicToRomanConversion(int input) {
        this(input, Stream.empty());
    }

    private ArabicToRomanConversion(int input, Stream<String> digits) {
        this.input = input;
        this.digits = digits;
    }

    private ArabicToRomanConversion apply(int arabic, String roman) {
        final var countX = input / arabic;
        final var newDigits = Stream
                .of(roman)
                .cycle(countX)
                .prependAll(digits);
        final var newInput = input % arabic;
        return new ArabicToRomanConversion(newInput, newDigits);
    }

    String getResult() {
        return digits.mkString();
    }

    ArabicToRomanConversion apply(Tuple2<Integer, String> digit) {
        return apply(digit._1, digit._2);
    }
}
