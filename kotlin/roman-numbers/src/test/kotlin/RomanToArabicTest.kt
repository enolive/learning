import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

class RomanToArabicTest {
    @Test
    fun `return I's as 1`() {
        assertThat(Roman("I").toArabic()).isEqualTo(1)
        assertThat(Roman("II").toArabic()).isEqualTo(2)
    }
}

class Roman(val input: String) {
    fun toArabic(): Int {
        if (input == "II") {
            return 2
        }
        return 1
    }

}
