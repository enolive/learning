import org.assertj.core.api.Assertions.assertThat
import org.junit.Test


class FizzBuzzTest {

    @Test
    fun `should return normal number itself`() {
        assertThat(generate(1)).isEqualTo("1")
        assertThat(generate(2)).isEqualTo("2")
    }

    @Test
    fun `should return numbers divisible by 3 as fizz`() {
        assertThat(generate(3)).isEqualTo("Fizz")
        assertThat(generate(6)).isEqualTo("Fizz")
    }

    @Test
    fun `should return numbers divisible by 5 as buzz`() {
        assertThat(generate(5)).isEqualTo("Buzz")
        assertThat(generate(10)).isEqualTo("Buzz")
    }

    @Test
    fun `should return numbers divisible by 3 and 5 as fizz-buzz`() {
        assertThat(generate(15)).isEqualTo("Fizz-Buzz")
    }

}
