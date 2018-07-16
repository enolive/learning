import io.vavr.Function1;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.math.BigDecimal;

import static io.vavr.API.*;

class HarryPotter {
    static BigDecimal getGroupPrice(int numberOfBooks) {
        return Function1.of(HarryPotter::groupPrice)
                        .andThen(BigDecimal::valueOf)
                        .apply(numberOfBooks);
    }

    private static double groupPrice(int books) {
        return Match(books).of(
                Case($(1), 8.0),
                Case($(2), 15.2),
                Case($(3), 21.6),
                Case($(4), 25.6),
                Case($(5), 30.0));
    }

    static List<BookSet> getBookSets(List<Book> books) {
        return books.groupBy(book -> book.volume)
                    .toList()
                    .map(HarryPotter::bookSetFromGrouping)
                    .sortBy(BookSet::getCount);
    }

    static List<Bundle> getBundles(List<BookSet> bookSets) {
        final var bundleSizes = Stream.unfoldLeft(Tuple.of(bookSets, 0), HarryPotter::tryCalculateBundleSize);
        return bundleSizes.zipWithIndex()
                          .map(HarryPotter::bundleFromSizeAndIndex)
                          .filter(Bundle::nonEmpty)
                          .toList();
    }

    private static Option<Tuple2<? extends Tuple2<List<BookSet>, Integer>, ? extends Integer>> tryCalculateBundleSize(
            Tuple2<List<BookSet>, Integer> remaining) {
        final var remainingSets = remaining._1;
        final var usedBooks = remaining._2;
        return tryCalculateBundleSize(remainingSets, usedBooks);
    }

    private static Option<Tuple2<? extends Tuple2<List<BookSet>, Integer>, ? extends Integer>> tryCalculateBundleSize(
            List<BookSet> remainingSets, Integer usedBooks) {
        return remainingSets.isEmpty()
                ? Option.none()
                : Option.of(calculateBundleSize(remainingSets, usedBooks));
    }

    private static Tuple2<Tuple2<List<BookSet>, Integer>, Integer> calculateBundleSize(List<BookSet> remainingSets, Integer usedBooks) {
        final var booksInCurrentSet = remainingSets.head().getCount();
        final var sizeOfCurrentBundle = booksInCurrentSet - usedBooks;
        final var nextSeed = Tuple.of(remainingSets.tail(), booksInCurrentSet);
        return Tuple.of(nextSeed, sizeOfCurrentBundle);
    }

    static List<Bundle> adjust(List<Bundle> bundles) {
        return null;
    }

    static BigDecimal getPrice(List<Book> books) {
        return null;
    }

    private static BookSet bookSetFromGrouping(Tuple2<Integer, List<Book>> tuple) {
        return new BookSet(tuple._1, tuple._2.length());
    }

    private static Bundle bundleFromSizeAndIndex(Tuple2<Integer, Integer> tuple) {
        return new Bundle(tuple._2 + 1, tuple._1);
    }
}
