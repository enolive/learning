import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

@DisplayName("Better name")
class DemoTest {
    @Test
    fun `framework should work`() {
        assertThat(true).isFalse()
    }

    @Nested
    @DisplayName("Nested tests work as well")
    inner class NestedTests {
        @Test
        fun `this should also work`() {
            assertThat(false).isTrue()
        }
    }
}