import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.collection.Seq;
import io.vavr.collection.Stream;
import io.vavr.control.Option;

import java.math.BigDecimal;

import static io.vavr.API.*;

class HarryPotter {
    static Seq<BookSet> getBookSets(Seq<Book> books) {
        return books
                .groupBy(Book::getVolume)
                .map(tuple -> new BookSet(tuple._1, tuple._2.length()));
    }

    static Seq<Bundle> getBundles(Seq<BookSet> bookSets) {
        return Stream
                .unfoldLeft(
                        new CreateBundle(bookSets),
                        HarryPotter::nextBundle)
                .head()
                .getBundles();
    }

    private static Option<Tuple2<? extends CreateBundle, ? extends CreateBundle>> nextBundle(CreateBundle previous) {
        if (previous.isEmpty()) {
            return Option.none();
        }
        final var next = previous.next();
        return Option(Tuple.of(next, next));
    }

    static Seq<BookSet> getRemainingSets(List<BookSet> bookSets) {
        return new CreateBundle(List.empty(), bookSets).next().getRemainingSets();
    }

    static BigDecimal getPrice(List<Book> books) {
        final var sets = getBookSets(books);
        final var bundles = getBundles(sets);
        final var adjusted = adjust(bundles);
        return adjusted
                .map(HarryPotter::getPrice)
                .foldLeft(BigDecimal.ZERO, BigDecimal::add);
    }

    static Seq<Bundle> adjust(Seq<Bundle> bundles) {
        final var bundlesWith3Or5 = bundles.filter(b -> b.has(3) || b.has(5));
        if (bundlesWith3Or5.length() == 2) {
            final var bundleWith3 = bundles.filter(b -> b.has(3)).head();
            final var bundleWith5 = bundles.filter(b -> b.has(5)).head();
            final var commonCount = Math.min(bundleWith3.getBundleCount(), bundleWith5.getBundleCount());
            final var remainingWith3 = new Bundle(3, bundleWith3.getBundleCount() - commonCount);
            final var remainingWith5 = new Bundle(5, bundleWith5.getBundleCount() - commonCount);
            return bundles.removeAll(bundlesWith3Or5)
                    .prependAll(List.of(remainingWith3, new Bundle(4, commonCount * 2), remainingWith5))
                    .removeAll(b -> b.getBundleCount() == 0);
        }
        return bundles;
    }

    private static BigDecimal getPrice(Bundle bundle) {
        return getGroupPrice(bundle.getBookCount()).multiply(BigDecimal.valueOf(bundle.getBundleCount()));
    }

    static BigDecimal getGroupPrice(int numberOfBooks) {
        return Match(numberOfBooks).of(
                Case($(1), BigDecimal.valueOf(8)),
                Case($(2), BigDecimal.valueOf(15.2)),
                Case($(3), BigDecimal.valueOf(21.6)),
                Case($(4), BigDecimal.valueOf(25.6)),
                Case($(5), BigDecimal.valueOf(30))
        );
    }
}
