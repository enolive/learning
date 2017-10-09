import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

class RomanToArabicTest {
    @Test
    fun `return I's as 1`() {
        assertThat(Roman("I").toArabic()).isEqualTo(1)
    }
}

class Roman(input: String) {
    fun toArabic(): Int {
        return 1
    }

}
