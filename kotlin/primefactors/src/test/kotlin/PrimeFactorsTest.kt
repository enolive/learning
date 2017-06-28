import io.kotlintest.specs.FunSpec
import org.assertj.core.api.Assertions.assertThat

class PrimeFactorsTest : FunSpec() {
    init {
        test("2 returns 2", {
            assertThat(PrimeFactors.of(2)).containsExactlyInAnyOrder(2)           
        })
        
        test("3 returns 3", {
            assertThat(PrimeFactors.of(3)).containsExactlyInAnyOrder(3)
        })
        
        test("4 returns 2,2", {
            assertThat(PrimeFactors.of(4)).containsExactlyInAnyOrder(2, 2)
        })

        test("5 returns 5", {
            assertThat(PrimeFactors.of(5)).containsExactlyInAnyOrder(5)
        })
        
        test("6 returns 3,2", {
            assertThat(PrimeFactors.of(6)).containsExactlyInAnyOrder(3, 2)  
        })
        
        test("8 returns 2,2,2", {
            assertThat(PrimeFactors.of(8)).containsExactlyInAnyOrder(2, 2, 2)
        })

        test("9 returns 3,3", {
            assertThat(PrimeFactors.of(9)).containsExactlyInAnyOrder(3, 3)
        })

        test("23 returns 23", {
            assertThat(PrimeFactors.of(23)).containsExactlyInAnyOrder(23)
        })

        test("42 returns 2,3,7", {
            assertThat(PrimeFactors.of(42)).containsExactlyInAnyOrder(2, 3, 7)
        })
    }

}

