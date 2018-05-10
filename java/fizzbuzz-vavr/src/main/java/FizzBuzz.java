import io.vavr.Function1;
import io.vavr.Function2;
import io.vavr.collection.Stream;

class FizzBuzz {

    private final Function1<Integer, String> seq = sequence().memoized();

    Stream<String> sequence() {
        var fizzes = Stream.of("", "", "Fizz").cycle();
        var buzzes = Stream.of("", "", "", "", "Buzz").cycle();
        var numbers = Stream.from(1).map(n -> Integer.toString(n));
        Function2<String, String, String> combine = (a, b) -> {
            if (a.isEmpty()) {
                return b;
            }
            if (b.isEmpty()) {
                return a;
            }
            return a + "-" + b;
        };
        Function2<String, String, String> choice = (a, b) ->
                a.isEmpty() ? b : a;
        var words = fizzes.zipWith(buzzes, combine);
        return words.zipWith(numbers, choice);
    }

    String single(int number) {
        return seq.apply(number - 1);
    }
}
