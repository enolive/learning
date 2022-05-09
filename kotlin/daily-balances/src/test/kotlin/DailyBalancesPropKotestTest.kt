import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.collections.shouldBeStrictlyIncreasingWith
import io.kotest.matchers.collections.shouldContainAll
import io.kotest.matchers.shouldBe
import io.kotest.property.Arb
import io.kotest.property.PropTestConfig
import io.kotest.property.arbitrary.*
import io.kotest.property.checkAll

class DailyBalancesPropKotestTest : DescribeSpec({
  describe("properties") {
    val arbBalance = arbitrary {
      val date = Arb.localDate().bind()
      val amount = Arb.bigDecimal().bind()
      Balance(date, amount)
    }
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
