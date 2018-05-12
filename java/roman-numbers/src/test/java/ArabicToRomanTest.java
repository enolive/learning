import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ArabicToRomanTest {

    private RomanConverter converter;

    @BeforeEach
    void setUp() {
        converter = new RomanConverter();
    }

    @Test
    void it_should_convert_1s_to_Is() {
        assertThat(converter.toRoman(1)).isEqualTo("I");
        assertThat(converter.toRoman(2)).isEqualTo("II");
        assertThat(converter.toRoman(3)).isEqualTo("III");
    }

    @Test
    void it_should_convert_5_to_V() {
        assertThat(converter.toRoman(5)).isEqualTo("V");
        assertThat(converter.toRoman(6)).isEqualTo("VI");
        assertThat(converter.toRoman(7)).isEqualTo("VII");
    }

    @Test
    void it_should_convert_10s_to_Xs() {
        assertThat(converter.toRoman(10)).isEqualTo("X");
        assertThat(converter.toRoman(18)).isEqualTo("XVIII");
        assertThat(converter.toRoman(20)).isEqualTo("XX");
        assertThat(converter.toRoman(36)).isEqualTo("XXXVI");
    }
}
