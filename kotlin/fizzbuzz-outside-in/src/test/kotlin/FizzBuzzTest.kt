import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test
import org.mockito.Mockito.*
import java.util.*

class FizzBuzzTest {
    @Nested
    inner class Algorithm {
        @Test
        fun `return input itself when no rule matches`() {
            val input = aNumber()
            val fizzBuzz = FizzBuzz(aRule(), aRule())
            assertThat(fizzBuzz.calculate(input)).isEqualTo(input.toString())
        }

        @Test
        fun `check if each rule applies to the input`() {
            val rule1 = aRule()
            val rule2 = aRule()
            val fizzBuzz = FizzBuzz(rule1, rule2)
            val input = aNumber()
            fizzBuzz.calculate(input)
            verify(rule1).appliesTo(input)
            verify(rule2).appliesTo(input)
        }

        @Test
        fun `return the result of the matching rule`() {
            val rule = aMatchingRule("Result")
            val fizzBuzz = FizzBuzz(rule)
            assertThat(fizzBuzz.calculate(aNumber())).isEqualTo("Result")
        }

        @Test
        fun `return the combination of multiple matching rules`() {
            val rule1 = aMatchingRule("Result1")
            val rule2 = aMatchingRule("Result2")
            val fizzBuzz = FizzBuzz(rule1, rule2)
            assertThat(fizzBuzz.calculate(aNumber())).isEqualTo("Result1-Result2")
        }

        private fun aMatchingRule(result: String): Rule {
            val rule = aRule()
            `when`(rule.appliesTo(anyInt())).thenReturn(true)
            `when`(rule.result).thenReturn(result)
            return rule
        }

        private fun aRule() = mock(Rule::class.java)

        private fun aNumber() = Math.abs(Random().nextInt(1000)) + 1
    }
    
    @Nested
    inner class ConcreteRules {
        @Test
        fun `return the expected values`() {
            val fizzBuzz = FizzBuzz()
            assertThat(fizzBuzz.calculate(1)).isEqualTo("1")
            assertThat(fizzBuzz.calculate(3)).isEqualTo("Fizz")
            assertThat(fizzBuzz.calculate(6)).isEqualTo("Fizz")
            assertThat(fizzBuzz.calculate(5)).isEqualTo("Buzz")
            assertThat(fizzBuzz.calculate(10)).isEqualTo("Buzz")
            assertThat(fizzBuzz.calculate(15)).isEqualTo("Fizz-Buzz")
        }
    }
}

