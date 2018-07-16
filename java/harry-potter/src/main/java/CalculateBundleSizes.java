import io.vavr.collection.List;
import io.vavr.collection.Stream;

class CalculateBundleSizes {
    private final List<BookSet> bookSets;
    private final int alreadyUsed;
    private final Stream<Integer> calculatedSizes;

    CalculateBundleSizes(List<BookSet> bookSets) {
        this(bookSets, 0, Stream.empty());
    }

    private CalculateBundleSizes(List<BookSet> bookSets, int alreadyUsed, Stream<Integer> calculatedSizes) {
        this.bookSets = bookSets;
        this.alreadyUsed = alreadyUsed;
        this.calculatedSizes = calculatedSizes;
    }

    Stream<Integer> invoke() {
        if (bookSets.isEmpty()) {
            return calculatedSizes;
        }
        final var numberOfCurrentBooks = bookSets.head().getCount();
        final var sizeOfCurrentBundle = numberOfCurrentBooks - alreadyUsed;
        final var remainingSets = bookSets.tail();
        return new CalculateBundleSizes(remainingSets, numberOfCurrentBooks, calculatedSizes.prepend(sizeOfCurrentBundle)).invoke();
    }
}
