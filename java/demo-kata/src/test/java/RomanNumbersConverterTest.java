import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.in;

class RomanNumbersConverterTest {

    private RomanNumbersConverter converter;

    @BeforeEach
    void setUp() {
        converter = new RomanNumbersConverter();
    }

    @Nested
    class ArabicToRoman {
        @Nested
        class AdditionRules {
            @Test
            void convertEmpty() {
                assertThat(converter.toRoman(0)).isEmpty();
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "1, I",
                    "2, II",
                    "3, III",
            })
            void convertToI(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "5, V",
                    "8, VIII",
            })
            void convertToV(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "10, X",
                    "17, XVII",
                    "30, XXX",
            })
            void convertToX(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }
        }

        @Nested
        class SubtractionRules {
            @Test
            void convertToIV() {
                assertThat(converter.toRoman(4)).isEqualTo("IV");
            }

            @Test
            void convertToIX() {
                assertThat(converter.toRoman(9)).isEqualTo("IX");
            }
        }
    }

    @Nested
    class RomanToArabic {
        @Nested
        class AdditionRules {
            @Test
            void convertFromEmpty() {
                assertThat(converter.toArabic("")).isEqualTo(0);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "I, 1",
                    "II, 2",
                    "III, 3",
            })
            void convertFromI(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "V, 5",
                    "VIII, 8",
            })
            void convertFromV(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "X, 10",
                    "XVII, 17",
                    "XXX, 30",
            })
            void convertFromX(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }
        }

        @Nested
        class SubtractionRules {
            @Test
            void convertFromIV() {
                assertThat(converter.toArabic("IV")).isEqualTo(4);
            }

            @Test
            void convertFromIX() {
                assertThat(converter.toArabic("IX")).isEqualTo(9);
            }
        }
    }

    @Nested
    class ConvertBackAndForth {
        @ParameterizedTest
        @ValueSource(ints = {
                5,
                42,
                1978,
                1234567890,
        })
        void numbersMatch(int input) {
            assertThat(converter.toArabic(converter.toRoman(input))).isEqualTo(input);
        }
    }
}
