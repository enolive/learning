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
        assertThat(converter.toArabic("VIII")).isEqualTo(8);
    }

    @Test
    void it_should_convert_Xs_to_10s() {
        assertThat(converter.toArabic("X")).isEqualTo(10);
        assertThat(converter.toArabic("XXVII")).isEqualTo(27);
    }

    @Test
    void it_should_convert_L_to_50() {
        assertThat(converter.toArabic("L")).isEqualTo(50);
        assertThat(converter.toArabic("LXXXVI")).isEqualTo(86);
    }
}
