import io.vavr.Function1;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.math.BigDecimal;

import static io.vavr.API.*;

class HarryPotter {
    static BigDecimal getPrice(List<Book> books) {
        return Function1.of(HarryPotter::getBookSets)
                        .andThen(HarryPotter::getBundles)
                        .andThen(HarryPotter::adjustBundles)
                        .andThen(HarryPotter::getPriceForBundles)
                        .apply(books);
    }

    static List<BookSet> getBookSets(List<Book> books) {
        return books.groupBy(Book::getVolume)
                    .toList()
                    .map(HarryPotter::bookSetFromGrouping)
                    .sortBy(BookSet::getCount);
    }

    static List<Bundle> getBundles(List<BookSet> bookSets) {
        return getBundleSizes(bookSets).zipWithIndex()
                                       .map(HarryPotter::bundleFromSizeAndIndex)
                                       .filter(Bundle::nonEmpty)
                                       .toList();
    }

    private static Stream<Integer> getBundleSizes(List<BookSet> bookSets) {
        return new CalculateBundleSizes(bookSets).invoke();
    }

    static List<Bundle> adjustBundles(List<Bundle> bundles) {
        final var bundlesWith3Books = bundlesWithNumberOfBooksOf(bundles, 3);
        final var bundlesWith5Books = bundlesWithNumberOfBooksOf(bundles, 5);
        return Option.of(Math.min(bundlesWith3Books, bundlesWith5Books))
                     .filter(commonSize -> commonSize != 0)
                     .map(commonSize -> replaceBundlesWith3And5By4(bundles, bundlesWith3Books, bundlesWith5Books, commonSize))
                     .getOrElse(bundles);
    }

    private static int bundlesWithNumberOfBooksOf(List<Bundle> bundles, int count) {
        return bundles.filter(bundle -> bundle.hasNumberOfDistinctBooks(count))
                      .map(Bundle::getCount)
                      .singleOption()
                      .getOrElse(0);
    }

    private static List<Bundle> replaceBundlesWith3And5By4(List<Bundle> bundles, int bundlesWith3Books, int bundlesWith5Books, int commonSize) {
        final var newSize3 = bundlesWith3Books - commonSize;
        final var newSize5 = bundlesWith5Books - commonSize;
        return bundles.removeAll(bundle -> bundle.hasNumberOfDistinctBooks(3))
                      .removeAll(bundle -> bundle.hasNumberOfDistinctBooks(5))
                      .append(new Bundle(3, newSize3))
                      .append(new Bundle(4, commonSize * 2))
                      .append(new Bundle(5, newSize5))
                      .filter(Bundle::nonEmpty);
    }

    private static BigDecimal getPriceForBundles(List<Bundle> adjusted) {
        return adjusted.map(HarryPotter::getBundlePrice)
                       .foldLeft(BigDecimal.ZERO, BigDecimal::add);
    }

    private static double groupPrice(int books) {
        return Match(books).of(
                Case($(1), 8.0),
                Case($(2), 15.2),
                Case($(3), 21.6),
                Case($(4), 25.6),
                Case($(5), 30.0));
    }

    private static BigDecimal getBundlePrice(Bundle bundle) {
        return getGroupPrice(bundle.getNumberOfDistinctBooks()).multiply(BigDecimal.valueOf(bundle.getCount()));
    }

    static BigDecimal getGroupPrice(int numberOfBooks) {
        return Function1.of(HarryPotter::groupPrice)
                        .andThen(BigDecimal::valueOf)
                        .apply(numberOfBooks);
    }

    private static BookSet bookSetFromGrouping(Tuple2<Integer, List<Book>> tuple) {
        return new BookSet(tuple._1, tuple._2.length());
    }

    private static Bundle bundleFromSizeAndIndex(Tuple2<Integer, Integer> tuple) {
        return new Bundle(tuple._2 + 1, tuple._1);
    }
}
