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

    @Test
    void it_should_convert_according_to_subtraction_rules() {
        assertThat(converter.toRoman(4)).isEqualTo("IV");
        assertThat(converter.toRoman(9)).isEqualTo("IX");
        assertThat(converter.toRoman(40)).isEqualTo("XL");
        assertThat(converter.toRoman(90)).isEqualTo("XC");
        assertThat(converter.toRoman(400)).isEqualTo("CD");
        assertThat(converter.toRoman(900)).isEqualTo("CM");
    }

    @Test
    void it_should_convert_50_to_L() {
        assertThat(converter.toRoman(50)).isEqualTo("L");
        assertThat(converter.toRoman(73)).isEqualTo("LXXIII");
    }

    @Test
    void it_should_convert_100s_to_Cs() {
        assertThat(converter.toRoman(100)).isEqualTo("C");
        assertThat(converter.toRoman(123)).isEqualTo("CXXIII");
    }

    @Test
    void it_should_convert_500_to_D() {
        assertThat(converter.toRoman(500)).isEqualTo("D");
        assertThat(converter.toRoman(857)).isEqualTo("DCCCLVII");
    }

    @Test
    void it_should_convert_1000s_to_Ms() {
        assertThat(converter.toRoman(1000)).isEqualTo("M");
        assertThat(converter.toRoman(1978)).isEqualTo("MCMLXXVIII");
    }
}
