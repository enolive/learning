import java.math.BigDecimal;

import static io.vavr.API.*;

class HarryPotter {
    static BigDecimal priceForGroup(int numberOfBooks) {
        return Match(numberOfBooks).of(
                Case($(1), BigDecimal.valueOf(8)),
                Case($(2), BigDecimal.valueOf(15.2)),
                Case($(3), BigDecimal.valueOf(21.6)),
                Case($(4), BigDecimal.valueOf(25.6)),
                Case($(5), BigDecimal.valueOf(30))
        );
    }
}
