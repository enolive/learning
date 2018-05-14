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
        assertThat(converter.toArabic("II")).isEqualTo(2);
        assertThat(converter.toArabic("III")).isEqualTo(3);
    }

    @Test
    void it_should_convert_V_to_5() {
        assertThat(converter.toArabic("V")).isEqualTo(5);
    }
}
