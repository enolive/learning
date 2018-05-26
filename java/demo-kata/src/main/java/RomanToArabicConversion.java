import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.CharSeq;
import io.vavr.collection.List;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.util.function.Function;

class RomanToArabicConversion {
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

    RomanToArabicConversion apply(Tuple2<Integer, String> digit) {
        return apply(digit._2, digit._1);
    }

    private RomanToArabicConversion apply(String roman, int arabic) {
        final var romansCount = Stream
                .unfoldLeft(
                        input,
                        createRemainingInput(CharSeq.of(roman).toList())
                )
                .length();
        final var remainingInput = input.drop(romansCount * roman.length());
        final var newResult = result + romansCount * arabic;
        return new RomanToArabicConversion(remainingInput, newResult);
    }

    private Function<List<Character>, Option<Tuple2<? extends List<Character>, ? extends Integer>>> createRemainingInput(List<Character> roman) {
        return remaining -> remaining.startsWith(roman)
                ? Option.of(Tuple.of(remaining.drop(roman.length()), 0))
                : Option.none();
    }
}
