import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class RomanToArabicTest {

    private RomanConverter converter;

    @BeforeEach
    void setUp() {
        converter = new RomanConverter();
    }

    @Test
    void it_should_convert_Is_to_1s() {
        assertThat(converter.toArabic("I")).isEqualTo(1);
    }
}
