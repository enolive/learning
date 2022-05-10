import io.kotest.matchers.collections.shouldBeStrictlyIncreasingWith
import io.kotest.matchers.collections.shouldContainAll
import io.kotest.matchers.shouldBe
import net.jqwik.api.*
import net.jqwik.api.constraints.NotEmpty
import net.jqwik.api.statistics.Statistics
import net.jqwik.kotlin.api.anyForType
import net.jqwik.kotlin.api.combine
import net.jqwik.time.api.Dates
import java.time.LocalDate

class DailyBalancesPropJqwikTest {
  @Property
  fun testArbitrary(@ForAll("uniqueBalances") list: List<Balance>) {
    Statistics.label("sizes").collect(list.size)
  }

  @Property
  fun `contains all original elements`(@ForAll("uniqueBalances") list: List<Balance>) {
    val result = list.expandToDaily()

    result.shouldContainAll(list)
  }

  @Property
  fun `result is strictly increasing by the balance date`(@ForAll("uniqueBalances") list: List<Balance>) {
    val result = list.expandToDaily()

    result.shouldBeStrictlyIncreasingWith(compareBy { it.date })
  }

  @Property
  fun `returns the whole range`(@ForAll("uniqueBalances") @NotEmpty list: List<Balance>) {
    val result = list.expandToDaily()

    result.map { it.date } shouldBe wholeDateRange(result.minOf { it.date }, result.maxOf { it.date })
  }

  @Provide
  fun uniqueBalances(): Arbitrary<List<Balance>> {
    val balanceArbitrary = combine(
      Dates.dates().between(LocalDate.of(1970, 1, 1), LocalDate.of(2030, 12, 31)),
      Arbitraries.bigDecimals()
    ) { d, a -> Balance(d, a) }
    return balanceArbitrary.list().uniqueElements { it.date }.ofMaxSize(100)
  }
}
