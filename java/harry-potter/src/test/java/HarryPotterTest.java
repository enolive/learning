import io.vavr.collection.List;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;
import java.util.Comparator;

import static org.assertj.core.api.Assertions.assertThat;

class HarryPotterTest {
    private static final Comparator<Bundle> BUNDLES_COMPARATOR = Comparator.comparing(Bundle::getNumberOfDistinctBooks)
                                                                           .thenComparing(Bundle::getCount);

    @ParameterizedTest
    @CsvSource(value = {
            "1, 8.0",
            "2, 15.2",
            "3, 21.6",
            "4, 25.6",
            "5, 30.0"
    })
    void priceGroup(int numberOfBooks, BigDecimal expected) {
        assertThat(HarryPotter.getGroupPrice(numberOfBooks)).isEqualByComparingTo(expected);
    }

    @Test
    void getBookSets() {
        final var books = List.of(
                new Book(1),
                new Book(1),
                new Book(1),
                new Book(2),
                new Book(2),
                new Book(3));
        final var bookSetComparator = Comparator.comparing(BookSet::getVolume)
                                                .thenComparing(BookSet::getCount);
        assertThat(HarryPotter.getBookSets(books))
                .as("books should be grouped by their volume and sorted by their count ascending")
                .usingElementComparator(bookSetComparator)
                .containsExactly(
                        new BookSet(3, 1),
                        new BookSet(2, 2),
                        new BookSet(1, 3)
                );
    }

    @Test
    void getBundles() {
        final var bookSets = List.of(
                new BookSet(1, 1),
                new BookSet(2, 1),
                new BookSet(3, 2),
                new BookSet(4, 2)
        );
        assertThat(HarryPotter.getBundles(bookSets))
                .as("book sets should be placed into bundles of distinct books")
                .usingElementComparator(BUNDLES_COMPARATOR)
                .containsExactly(
                        new Bundle(2, 1),
                        new Bundle(4, 1)
                );
    }

    @Test
    void adjust() {
        assertThat(HarryPotter.adjust(List.of(new Bundle(3, 1))))
                .usingElementComparator(BUNDLES_COMPARATOR)
                .containsExactly(new Bundle(3, 1));
        assertThat(HarryPotter.adjust(List.of(new Bundle(3, 1), new Bundle(5, 1))))
                .usingElementComparator(BUNDLES_COMPARATOR)
                .containsExactly(new Bundle(4, 2));
        assertThat(HarryPotter.adjust(List.of(new Bundle(3, 2), new Bundle(5, 2))))
                .usingElementComparator(BUNDLES_COMPARATOR)
                .containsExactly(new Bundle(4, 4));
        assertThat(HarryPotter.adjust(List.of(new Bundle(3, 3), new Bundle(5, 1))))
                .usingElementComparator(BUNDLES_COMPARATOR)
                .containsExactly(new Bundle(3, 2), new Bundle(4, 2));
    }

    @Test
    void singleDiscount() {
        assertThat(HarryPotter.getPrice(books(1))).isEqualByComparingTo("8.00");
        assertThat(HarryPotter.getPrice(books(1, 2))).isEqualByComparingTo("15.20");
        assertThat(HarryPotter.getPrice(books(1, 2, 3))).isEqualByComparingTo("21.60");
        assertThat(HarryPotter.getPrice(books(1, 2, 3, 4))).isEqualByComparingTo("25.60");
        assertThat(HarryPotter.getPrice(books(1, 2, 3, 4, 5))).isEqualByComparingTo("30.00");
    }

    private List<Book> books(int... volumes) {
        return List.ofAll(volumes).map(Book::new);
    }

    @Disabled
    @Test
    void multipleDiscount() {
        assertThat(HarryPotter.getPrice(books(1, 1, 2, 2))).isEqualByComparingTo("30.40");
        assertThat(HarryPotter.getPrice(books(1, 1, 2, 3))).isEqualByComparingTo("29.60");
    }

    @Disabled
    @Test
    void edgeCases() {
        assertThat(HarryPotter.getPrice(books(1, 1, 2, 2, 3, 3, 4, 5))).isEqualByComparingTo("51.20");
        assertThat(HarryPotter.getPrice(books(
                1, 1, 1, 1,
                2, 2, 2, 2,
                3, 3, 3, 3,
                4, 4, 4,
                5, 5, 5))).isEqualByComparingTo("111.20");
    }
}
