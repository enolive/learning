import io.vavr.Function1;
import io.vavr.Tuple2;
import io.vavr.collection.List;

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
                    .map(HarryPotter::fromTuple)
                    .sortBy(BookSet::getCount);
    }

    private static BookSet fromTuple(Tuple2<Integer, List<Book>> tuple) {
        return new BookSet(tuple._1, tuple._2.length());
    }

    static List<Bundle> getBundles(List<BookSet> bookSets) {
        return null;
    }

    static List<Bundle> adjust(List<Bundle> bundles) {
        return null;
    }

    static BigDecimal getPrice(List<Book> books) {
        return null;
    }
}
