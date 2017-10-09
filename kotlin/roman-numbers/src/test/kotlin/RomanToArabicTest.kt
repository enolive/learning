import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

class RomanToArabicTest {
    @Test
    fun `return I's as 1`() {
        assertThat(Roman("I").toArabic()).isEqualTo(1)
        assertThat(Roman("II").toArabic()).isEqualTo(2)
        assertThat(Roman("III").toArabic()).isEqualTo(3)
    }

    @Test
    fun `return V's as 5`() {
        assertThat(Roman("V").toArabic()).isEqualTo(5)
        assertThat(Roman("VIII").toArabic()).isEqualTo(8)
    }
}