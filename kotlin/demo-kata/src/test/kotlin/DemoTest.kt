import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class DemoTest {
    @Test
    fun `framework should work`() {
        assertThat(true).isFalse()
    }
    
    @Nested
    inner class `Nested tests work as well` {
        @Test
        fun `this should also work`() {
            assertThat(false).isTrue()
        }
    }
}