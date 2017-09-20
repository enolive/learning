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
        assertThat(Arabic(3).toRoman()).isEqualTo("III")
    }

    @Test
    fun `convert should return V's for every number less than 9`() {
        assertThat(Arabic(5).toRoman()).isEqualTo("V")
        assertThat(Arabic(8).toRoman()).isEqualTo("VIII")
    }

    @Test
    internal fun `convert should return X's for every number less than 40`() {
        assertThat(Arabic(10).toRoman()).isEqualTo("X")
    }
}