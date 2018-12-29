package de.welcz.fizzbuzz;

import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ExtendWith(SpringExtension.class)
class FizzBuzzServiceTest {
    @Autowired
    private FizzBuzzService service;

    @ParameterizedTest
    @CsvSource(value = {
            "1, 1",
            "2, 2",
    })
    void replicates_normal_numbers(int input, String expected) {
        assertThat(service.calculate(input)).isEqualTo(expected);
    }

    @ParameterizedTest
    @ValueSource(ints = {3, 6})
    void fizz_numbers_divisible_by_3(int input) {
        assertThat(service.calculate(input)).isEqualTo("Fizz");
    }

    @ParameterizedTest
    @ValueSource(ints = {5, 10})
    void buzz_numbers_divisible_by_5(int input) {
        assertThat(service.calculate(input)).isEqualTo("Buzz");
    }

    @ParameterizedTest
    @ValueSource(ints = {15, 30})
    void fizz_buzz_numbers_divisible_by_3_and_5(int input) {
        assertThat(service.calculate(input)).isEqualTo("Fizz-Buzz");
    }
}
