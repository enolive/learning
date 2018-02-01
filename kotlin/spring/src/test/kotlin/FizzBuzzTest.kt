import fizz.buzz.FizzBuzzInKotlin
import org.assertj.core.api.Assertions.assertThat
import org.junit.Before
import org.junit.Test

class FizzBuzzTest {
    private lateinit var service: FizzBuzzInKotlin

    @Before
    fun setUp() {
        service = FizzBuzzInKotlin()
    }

    @Test
    fun `it should calculate the number itself`() {
        assertThat(service.calculate(1)).isEqualTo("1")
        assertThat(service.calculate(2)).isEqualTo("2")
    }

    @Test
    fun `it should calculate Fizz for numbers divisible by 3`() {
        assertThat(service.calculate(3)).isEqualTo("Fizz")
        assertThat(service.calculate(6)).isEqualTo("Fizz")
    }

    @Test
    fun `it should calculate Buzz for numbers divisible by 5`() {
        assertThat(service.calculate(5)).isEqualTo("Buzz")
        assertThat(service.calculate(10)).isEqualTo("Buzz")
    }

    @Test
    fun `it should calculate Zazz for numbers divisible by 7`() {
        assertThat(service.calculate(7)).isEqualTo("Zazz")
        assertThat(service.calculate(14)).isEqualTo("Zazz")
    }

    @Test
    fun `it should calculate Fizz-Buzz-Zazz`() {
        assertThat(service.calculate(105)).isEqualTo("Fizz-Buzz-Zazz")
    }

    @Test
    fun `it should calculate up to Fizz-Buzz-Zazz`() {
        val result = service.calculateUpTo(15)
        assertThat(result).containsExactly(
                "1",
                "2",
                "Fizz",
                "4",
                "Buzz",
                "Fizz",
                "Zazz",
                "8",
                "Fizz",
                "Buzz",
                "11",
                "Fizz",
                "13",
                "Zazz",
                "Fizz-Buzz")
    }
}