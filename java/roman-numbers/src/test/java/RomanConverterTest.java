import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

class RomanConverterTest {

    private RomanConverter converter;

    @BeforeEach
    void setUp() {
        converter = new RomanConverter();
    }

    @Nested
    class ArabicToRoman {
        @Nested
        class AdditionRules {
            @ParameterizedTest
            @CsvSource(value = {
                    "1, I",
                    "2, II",
                    "3, III",
            })
            void it_should_convert_1s_to_Is(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "5, V",
                    "6, VI",
                    "7, VII",
            })
            void it_should_convert_5_to_V(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "10, X",
                    "18, XVIII",
                    "20, XX",
                    "36, XXXVI",
            })
            void it_should_convert_10s_to_Xs(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "50, L",
                    "73, LXXIII",
            })
            void it_should_convert_50_to_L(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "100, C",
                    "123, CXXIII",
            })
            void it_should_convert_100s_to_Cs(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);

            }

            @ParameterizedTest
            @CsvSource(value = {
                    "500, D",
                    "857, DCCCLVII",
            })
            void it_should_convert_500_to_D(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "1000, M",
                    "1978, MCMLXXVIII",
            })
            void it_should_convert_1000s_to_Ms(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }
        }

        @Nested
        class SubtractionRules {
            @ParameterizedTest
            @CsvSource(value = {
                    "4, IV",
                    "9, IX",
                    "40, XL",
                    "90, XC",
                    "400, CD",
                    "900, CM",
            })
            void it_should_convert_according_to_subtraction_rules(int input, String expected) {
                assertThat(converter.toRoman(input)).isEqualTo(expected);
            }
        }
    }

    @Nested
    class RomanToArabic {

        private RomanConverter converter;

        @BeforeEach
        void setUp() {
            converter = new RomanConverter();
        }

        @Nested
        class AdditionRules {
            @ParameterizedTest
            @CsvSource(value = {
                    "I, 1",
                    "II, 2",
                    "III, 3",
            })
            void it_should_convert_Is_to_1s(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "V, 5",
                    "VIII, 8",
            })
            void it_should_convert_V_to_5(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "X, 10",
                    "XXVII, 27",
            })
            void it_should_convert_Xs_to_10s(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "L, 50",
                    "LXXXVI, 86",
            })
            void it_should_convert_L_to_50(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "C, 100",
                    "CLXVI, 166",
            })
            void it_should_convert_Cs_to_100s(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "D, 500",
                    "DCLXVI, 666",
            })
            void it_should_convert_D_to_500(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }

            @ParameterizedTest
            @CsvSource(value = {
                    "M, 1000",
                    "MCMLXXVIII, 1978",
            })
            void it_should_convert_Ms_to_1000s(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }
        }
        @Nested
        class SubtractionRules {
            @ParameterizedTest
            @CsvSource(value = {
                    "IV, 4",
                    "IX, 9",
                    "XL, 40",
                    "XC, 90",
                    "CD, 400",
                    "CM, 900",
            })
            void it_should_convert_according_to_subtraction_rules(String input, int expected) {
                assertThat(converter.toArabic(input)).isEqualTo(expected);
            }
        }
    }

    @Nested
    class BackAndForth {
        @ParameterizedTest
        @ValueSource(ints = {
                0,
                1,
                1978,
                1234567890
        })
        void it_should_convert_an_arabic_value_back_and_forth(int input) {
            assertThat(converter.toArabic(converter.toRoman(input))).isEqualTo(input);
        }
    }
}