import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

class CalculateBundleSizes {
    private final List<BookSet> bookSets;
    private final int alreadyUsed;

    CalculateBundleSizes(List<BookSet> bookSets) {
        this(bookSets, 0);
    }

    private CalculateBundleSizes(List<BookSet> bookSets, int alreadyUsed) {
        this.bookSets = bookSets;
        this.alreadyUsed = alreadyUsed;
    }

    Stream<Integer> invoke() {
        return Stream.unfoldLeft(this, seed -> Option.of(seed)
                                                     .filter(s -> s.bookSets.nonEmpty())
                                                     .map(this::calculateNext));
    }

    private Tuple2<? extends CalculateBundleSizes, ? extends Integer> calculateNext(CalculateBundleSizes seed) {
        final var numberOfCurrentBooks = seed.bookSets.head().getCount();
        final var sizeOfCurrentBundle = numberOfCurrentBooks - seed.alreadyUsed;
        final var remainingSets = seed.bookSets.tail();
        final var newSeed = new CalculateBundleSizes(remainingSets, numberOfCurrentBooks);
        return Tuple.of(newSeed, sizeOfCurrentBundle);
    }

}
