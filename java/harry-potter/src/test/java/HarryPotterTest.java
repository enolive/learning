import io.vavr.collection.List;
import io.vavr.collection.Seq;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class HarryPotterTest {
    @ParameterizedTest
    @CsvSource(value = {
            "1, 8.0",
            "2, 15.2",
            "3, 21.6",
            "4, 25.6",
            "5, 30.0"
    })
    void priceGroup(int numberOfBooks, BigDecimal expected) {
        assertThat(HarryPotter.priceForGroup(numberOfBooks)).isEqualByComparingTo(expected);
    }

    @Test
    void getBookSets() {
        assertThat(getBookSets(List.of(
                new Book(1),
                new Book(1),
                new Book(1),
                new Book(2),
                new Book(2),
                new Book(3))))
                .containsExactly(
                        new BookSet(1, 3),
                        new BookSet(2, 2),
                        new BookSet(3, 1)
                );
    }

    private Seq<BookSet> getBookSets(Seq<Book> books) {
        return books
                .groupBy(Book::getVolume)
                .map(tuple -> new BookSet(tuple._1, tuple._2.length()));
    }
}
