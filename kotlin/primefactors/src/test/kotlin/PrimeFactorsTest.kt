import io.kotlintest.specs.FunSpec
import org.assertj.core.api.Assertions.assertThat

class PrimeFactorsTest : FunSpec() {
    var factors = PrimeFactors()

    init {
        test("2 returns 2", {
            assertThat(factors.split(2)).containsExactlyInAnyOrder(2)           
        })
        
        test("3 returns 3", {
            assertThat(factors.split(3)).containsExactlyInAnyOrder(3)
        })
        
        test("4 returns 2,2", {
            assertThat(factors.split(4)).containsExactlyInAnyOrder(2, 2)
        })
        
        test("6 returns 3,2", {
            assertThat(factors.split(6)).containsExactlyInAnyOrder(3, 2)  
        })
        
        test("8 returns 2,2,2", {
            assertThat(factors.split(8)).containsExactlyInAnyOrder(2, 2, 2)
        })

        test("9 returns 3,3", {
            assertThat(factors.split(9)).containsExactlyInAnyOrder(3, 3)
        })
    }

}

