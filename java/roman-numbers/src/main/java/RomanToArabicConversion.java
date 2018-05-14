import io.vavr.Tuple2;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.util.function.Function;

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
        final var numberOfRomans = Stream
                .unfoldLeft(input, makeTuple(rule))
                .length();
        final var newResult = numberOfRomans * rule.getArabic() + result;
        final var offset = numberOfRomans * rule.getRoman().length();
        final var newInput = input.substring(offset);
        return new RomanToArabicConversion(newInput, newResult);
    }

    private Function<String, Option<Tuple2<? extends String, ? extends Integer>>> makeTuple(Rule rule) {
        return remaining -> remaining.startsWith(rule.getRoman())
                        ? Option.of(new Tuple2<>(remaining.substring(rule.getRoman().length()), 0))
                        : Option.none();
    }
}
