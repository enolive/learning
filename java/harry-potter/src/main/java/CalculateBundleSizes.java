import io.vavr.collection.List;
import io.vavr.collection.Stream;

class CalculateBundleSizes {
    private final List<BookSet> bookSets;

    CalculateBundleSizes(List<BookSet> bookSets) {
        this.bookSets = bookSets;
    }

    Stream<Integer> invoke() {
        return invoke(0, Stream.empty());
    }

    private Stream<Integer> invoke(int alreadyUsed, Stream<Integer> acc) {
        if (bookSets.isEmpty()) {
            return acc;
        }
        final var numberOfCurrentBooks = bookSets.head().getCount();
        final var sizeOfCurrentBundle = numberOfCurrentBooks - alreadyUsed;
        final var remainingSets = bookSets.tail();
        return new CalculateBundleSizes(remainingSets).invoke(numberOfCurrentBooks, acc.prepend(sizeOfCurrentBundle));
    }
}
