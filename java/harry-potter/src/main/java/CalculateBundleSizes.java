import io.vavr.collection.List;
import io.vavr.collection.Stream;

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
        return invoke(Stream.empty());
    }

    private Stream<Integer> invoke(Stream<Integer> calculatedSizes) {
        if (bookSets.isEmpty()) {
            return calculatedSizes;
        }
        final var numberOfCurrentBooks = bookSets.head().getCount();
        final var sizeOfCurrentBundle = numberOfCurrentBooks - alreadyUsed;
        final var remainingSets = bookSets.tail();
        return new CalculateBundleSizes(remainingSets, numberOfCurrentBooks).invoke(calculatedSizes.prepend(sizeOfCurrentBundle));
    }
}
