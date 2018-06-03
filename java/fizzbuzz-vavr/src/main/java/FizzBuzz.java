import io.vavr.Function1;
import io.vavr.collection.Stream;

class FizzBuzz {

    private final Function1<Integer, String> seq = sequence().memoized();

    private static String combine(String a, String b) {
        if (a.isEmpty()) {
            return b;
        }
        if (b.isEmpty()) {
            return a;
        }
        return a + "-" + b;
    }

    private static String choice(String a, String b) {
        return a.isEmpty() ? b : a;
    }

    Stream<String> sequence() {
        final var fizzes = Stream.of("", "", "Fizz").cycle();
        final var buzzes = Stream.of("", "", "", "", "Buzz").cycle();
        final var numbers = Stream.from(1).map(n -> Integer.toString(n));
        final var words = fizzes.zipWith(buzzes, FizzBuzz::combine);
        return words.zipWith(numbers, FizzBuzz::choice);
    }

    String single(int number) {
        return seq.apply(number - 1);
    }
}
