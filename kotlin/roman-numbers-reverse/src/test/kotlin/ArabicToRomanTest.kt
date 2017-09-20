import org.assertj.core.api.Assertions.*
import org.junit.jupiter.api.Test

class ArabicToRomanTest {
    @Test
    fun `convert zero should return empty string`() {
        assertThat(Arabic(0).toRoman()).isEqualTo("")
    }

    @Test
    fun `convert should return I's for every number less than 4`() {
        assertThat(Arabic(1).toRoman()).isEqualTo("I")
        assertThat(Arabic(2).toRoman()).isEqualTo("II")
    }
}