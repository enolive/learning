import org.junit.Test
import org.assertj.core.api.Assertions.*

class ArabicToRomanTest {
    @Test
    fun `convert zero should return empty string`() {
        assertThat(Arabic(0).toRoman()).isEqualTo("")
    }
}