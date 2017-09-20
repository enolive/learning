import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class `arabic to roman conversion` {
    @Nested
    inner class `summation rules` {
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
        fun `convert should return V's for every number from 5 up to 9`() {
            assertThat(Arabic(5).toRoman()).isEqualTo("V")
            assertThat(Arabic(6).toRoman()).isEqualTo("VI")
            assertThat(Arabic(8).toRoman()).isEqualTo("VIII")
        }

        @Test
        fun `convert should return X's for every number from 10 up to 40`() {
            assertThat(Arabic(10).toRoman()).isEqualTo("X")
            assertThat(Arabic(35).toRoman()).isEqualTo("XXXV")
        }

        @Test
        fun `convert should return L's for every number from 50 up to 90`() {
            assertThat(Arabic(50).toRoman()).isEqualTo("L")
            assertThat(Arabic(73).toRoman()).isEqualTo("LXXIII")
        }

        @Test
        fun `convert should return C's for every number from 100 up to 400`() {
            assertThat(Arabic(100).toRoman()).isEqualTo("C")
            assertThat(Arabic(352).toRoman()).isEqualTo("CCCLII")
        }

        @Test
        fun `convert should return D's for every number from 500 up to 900`() {
            assertThat(Arabic(500).toRoman()).isEqualTo("D")
            assertThat(Arabic(666).toRoman()).isEqualTo("DCLXVI")
        }

        @Test
        fun `convert should return M's for every number from 1000`() {
            assertThat(Arabic(1000).toRoman()).isEqualTo("M")
            assertThat(Arabic(1236).toRoman()).isEqualTo("MCCXXXVI")
        }
    }
    
    @Nested
    inner class `subtraction rules` {
        @Test
        fun `convert should return IX for 9`() {
            assertThat(Arabic(9).toRoman()).isEqualTo("IX")
        }

        @Test
        fun `convert should return IV for 4`() {
            assertThat(Arabic(4).toRoman()).isEqualTo("IV")
        }

        @Test
        fun `convert should return XL for 40`() {
            assertThat(Arabic(40).toRoman()).isEqualTo("XL")
        }

        @Test
        fun `convert should return XC for 90`() {
            assertThat(Arabic(90).toRoman()).isEqualTo("XC")
        }

        @Test
        fun `convert should return CD for 400`() {
            assertThat(Arabic(400).toRoman()).isEqualTo("CD")
        }

        @Test
        fun `convert should return CM for 900`() {
            assertThat(Arabic(900).toRoman()).isEqualTo("CM")
        }
    }
    
    @Nested
    inner class `high level tests` {
        @Test
        fun `it should convert 1978`() {
            assertThat(Arabic(1978).toRoman()).isEqualTo("MCMLXXVIII")
        }

        @Test
        fun `it should convert 1984`() {
            assertThat(Arabic(1984).toRoman()).isEqualTo("MCMLXXXIV")
        }
    }
}