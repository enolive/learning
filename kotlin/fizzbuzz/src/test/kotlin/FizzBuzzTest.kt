import io.kotlintest.specs.FunSpec
import org.assertj.core.api.Assertions.assertThat

class FizzBuzzTest : FunSpec() {
    private var target = Calculator()

    init {
        test("normal numbers should be returned as is", {
            assertThat(target.next(1)).isEqualTo("1")
            assertThat(target.next(2)).isEqualTo("2")
        })

        test("numbers divisible by 3 should return Fizz", {
            assertThat(target.next(3)).isEqualTo("Fizz")
            assertThat(target.next(6)).isEqualTo("Fizz")
        })
        
        test("numbers divisible by 5 should return Buzz", {
            assertThat(target.next(5)).isEqualTo("Buzz")
            assertThat(target.next(10)).isEqualTo("Buzz")
        })
        
        test("numbers divisible by 3 and 5 should return Fizz-Buzz", {
            assertThat(target.next(15)).isEqualTo("Fizz-Buzz")
            assertThat(target.next(30)).isEqualTo("Fizz-Buzz")
        })
    }
}

