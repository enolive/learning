import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class FizzBuzzTest {

    private FizzBuzz fizzBuzz;

    @BeforeEach
    void setUp() {
        fizzBuzz = new FizzBuzz();
    }

    @Test
    void normal_numbers_should_be_returned_as_is() {
        assertThat(fizzBuzz.single(1)).isEqualTo("1");
        assertThat(fizzBuzz.single(2)).isEqualTo("2");
    }

    @Test
    void numbers_divisible_by_3_should_be_returned_as_fizz() {
        assertThat(fizzBuzz.single(3)).isEqualTo("Fizz");
        assertThat(fizzBuzz.single(6)).isEqualTo("Fizz");
    }

    @Test
    void numbers_divisible_by_5_should_be_returned_as_buzz() {
        assertThat(fizzBuzz.single(5)).isEqualTo("Buzz");
        assertThat(fizzBuzz.single(10)).isEqualTo("Buzz");
    }

    @Test
    void numbers_divisible_by_3_and_5_should_be_returned_as_fizz_buzz() {
        assertThat(fizzBuzz.single(15)).isEqualTo("Fizz-Buzz");
        assertThat(fizzBuzz.single(30)).isEqualTo("Fizz-Buzz");
    }

    @Test
    void seuence_should_contain_the_expected_values() {
        assertThat(fizzBuzz.sequence().take(15))
                .containsExactly("1", "2", "Fizz", "4",
                        "Buzz", "Fizz", "7", "8",
                        "Fizz", "Buzz", "11",
                        "Fizz", "13", "14", "Fizz-Buzz");
    }
}
