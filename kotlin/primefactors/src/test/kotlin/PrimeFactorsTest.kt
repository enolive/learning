import io.kotlintest.specs.FunSpec
import org.assertj.core.api.Assertions.assertThat

class PrimeFactorsTest : FunSpec() {
    init {
        var factors = PrimeFactors().split()
        
        test("2 returns 2", {
            assertThat(factors).containsExactly(2)           
        })
    }

}

