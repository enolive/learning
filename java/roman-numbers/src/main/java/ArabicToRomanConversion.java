import io.vavr.Tuple2;
import io.vavr.collection.Stream;

final class ArabicToRomanConversion {
    private final int remainingInput;
    private final Stream<String> digits;

    ArabicToRomanConversion(int remainingInput) {
        this(remainingInput, Stream.empty());
    }

    private ArabicToRomanConversion(int remainingInput, Stream<String> digits) {
        this.remainingInput = remainingInput;
        this.digits = digits;
    }

    String getResult() {
        return digits.mkString();
    }

    ArabicToRomanConversion apply(Tuple2<Integer, String> rule) {
        return apply(rule._1, rule._2);
    }

    private ArabicToRomanConversion apply(Integer arabic, String roman) {
        final var numberOfDigits = remainingInput / arabic;
        final var newDigits = Stream.of(roman).cycle(numberOfDigits).prependAll(digits);
        final var newInput = remainingInput % arabic;
        return new ArabicToRomanConversion(newInput, newDigits);
    }
}