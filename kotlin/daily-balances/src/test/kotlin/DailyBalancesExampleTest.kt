import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.shouldBe
import java.time.LocalDate

class DailyBalancesExampleTest : DescribeSpec({
  describe("generating daily dates") {
    it("leaves empty list untouched") {
      val list: List<Balance> = emptyList()

      val result = list.expandToDaily()

      result.shouldBeEmpty()
    }

    it("leaves already discrete list untouched") {
      val start = LocalDate.now()
      val list = start
        .datesUntil(start.plusDays(5))
        .toList()
        .mapIndexed { index, date -> Balance(date, index.toBigDecimal() * 1000.toBigDecimal()) }

      val result = list.expandToDaily()

      result shouldBe list
    }

    it("fills dates between") {
      val list = listOf(
        Balance(LocalDate.parse("2022-05-01"), 100.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-04"), 200.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-08"), 50.toBigDecimal()),
      ).shuffled()
      val expected = listOf(
        Balance(LocalDate.parse("2022-05-01"), 100.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-02"), 100.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-03"), 100.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-04"), 200.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-05"), 200.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-06"), 200.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-07"), 200.toBigDecimal()),
        Balance(LocalDate.parse("2022-05-08"), 50.toBigDecimal()),
      )

      val result = list.expandToDaily()

      result shouldBe expected
    }
  }
})

