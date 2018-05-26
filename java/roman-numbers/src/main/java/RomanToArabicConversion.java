import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.CharSeq;
import io.vavr.collection.List;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.util.function.Function;

final class RomanToArabicConversion {
    private static final int DOES_NOT_MATTER = 42;
    private final List<Character> input;
    private final int result;

    RomanToArabicConversion(String input) {
        this(CharSeq.of(input).toList(), 0);
    }

    private RomanToArabicConversion(List<Character> input, int result) {
        this.input = input;
        this.result = result;
    }

    int getResult() {
        return result;
    }

    RomanToArabicConversion apply(Tuple2<Integer, String> rule) {
        return apply(rule._1, CharSeq.of(rule._2).toList());
    }

    private RomanToArabicConversion apply(Integer arabic, List<Character> roman) {
        final var numberOfRomans = Stream
                .unfoldLeft(input, tryCreateDigit(roman))
                .length();
        final var newResult = result + numberOfRomans * arabic;
        final var newInput = input.drop(numberOfRomans * roman.length());
        return new RomanToArabicConversion(newInput, newResult);
    }

    private Function<List<Character>, Option<Tuple2<? extends List<Character>, ? extends Integer>>> tryCreateDigit(List<Character> roman) {
        return remaining -> remaining.startsWith(roman)
                ? Option.of(Tuple.of(remaining.drop(roman.length()), DOES_NOT_MATTER))
                : Option.none();
    }
}
