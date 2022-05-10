import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.booleans.shouldBeFalse
import io.kotest.matchers.collections.shouldBeStrictlyIncreasingWith
import io.kotest.matchers.collections.shouldContainAll
import io.kotest.matchers.shouldBe
import io.kotest.property.Arb
import io.kotest.property.PropTestConfig
import io.kotest.property.arbitrary.*
import io.kotest.property.checkAll
import java.time.LocalDate

class DailyBalancesPropKotestTest : DescribeSpec({
  describe("properties") {
    val arbBalance = Arb.bind(Arb.localDate(), Arb.bigDecimal()) { d, a -> Balance(d, a) }
    fun arbUniqueList(minLength: Int = 0) = Arb.list(arbBalance, minLength..100).map { it.distinctBy { it.date } }

    it("contains all original elements") {
      checkAll(PropTestConfig(outputClassifications = true), arbUniqueList()) { list ->
        val result = list.expandToDaily()

        result.shouldContainAll(list)
      }
    }

    it("result is strictly increasing by the balance date") {
      checkAll(arbUniqueList()) { list ->
        val result = list.expandToDaily()

        result.shouldBeStrictlyIncreasingWith(compareBy { it.date })
      }
    }

    it("returns the whole range") {
      checkAll(arbUniqueList(minLength = 1)) { list ->
        val result = list.expandToDaily()

        result.map { it.date } shouldBe wholeDateRange(result.minOf { it.date }, result.maxOf { it.date })
      }
    }
  }
})
