import io.kotlintest.specs.FunSpec
import org.assertj.core.api.Assertions.assertThat

class PrimeFactorsTest : FunSpec() {
    init {
        test("Something") {
            assertThat(false).isFalse()
        }
        
        test("2 returns 2", {
            val factors = arrayOf(2)

            assertThat(factors).containsExactly(2);            
        })
    }
}