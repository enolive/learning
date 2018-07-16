import io.vavr.collection.List;

import java.math.BigDecimal;

import static io.vavr.API.*;

class HarryPotter {
    static BigDecimal getGroupPrice(int numberOfBooks) {
        return Match(numberOfBooks).of(
                Case($(1), BigDecimal.valueOf(8.0)),
                Case($(2), BigDecimal.valueOf(15.2)),
                Case($(3), BigDecimal.valueOf(21.6)),
                Case($(4), BigDecimal.valueOf(25.6)),
                Case($(5), BigDecimal.valueOf(30.0))
        );
    }

    static List<BookSet> getBookSets(List<Book> books) {
        return null;
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
