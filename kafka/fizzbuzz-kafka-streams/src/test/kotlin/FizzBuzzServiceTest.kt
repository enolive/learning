import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

class FizzBuzzServiceTest {
    private lateinit var fizzBuzzService: FizzBuzzService

    @BeforeEach
    internal fun setUp() {
        fizzBuzzService = FizzBuzzService()
    }

    @ParameterizedTest
    @ValueSource(ints = [1, 2, 4])
    internal fun `replicate default number`(input: Int) {
        assertThat(fizzBuzzService.single(input)).isEqualTo(input.toString())
    }

    @ParameterizedTest
    @ValueSource(ints = [3, 6, 9])
    internal fun `replace number divisible by 3 with 'Fizz'`(input: Int) {
        assertThat(fizzBuzzService.single(input)).isEqualTo("Fizz")
    }

    @ParameterizedTest
    @ValueSource(ints = [5, 10])
    internal fun `replace number divisible by 5 with 'Buzz'`(input: Int) {
        assertThat(fizzBuzzService.single(input)).isEqualTo("Buzz")
    }

    @ParameterizedTest
    @ValueSource(ints = [15, 30])
    internal fun `replace number divisible by 3 and 5 with 'Fizz-Buzz'`(input: Int) {
        assertThat(fizzBuzzService.single(input)).isEqualTo("Fizz-Buzz")
    }

    @Test
    internal fun `create expected sequence`() {
        assertThat(fizzBuzzService.sequenceUpTo(15)).containsExactly(
                "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8",
                "Fizz", "Buzz", "11", "Fizz", "13", "14", "Fizz-Buzz")
    }
}

