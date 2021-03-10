import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import java.math.BigDecimal
import java.time.LocalDate

class AmountPerDateTest : DescribeSpec({
  describe("aggregate amounts per date") {
    it("groups payments by date") {
      val payments = listOf(
        Payment(amount = 2.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        Payment(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        Payment(amount = 6.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
        Payment(amount = 4.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        Payment(amount = 5.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
        Payment(amount = 3.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
      )
      val expected = listOf(
        DeltaAmountPerDate(deltaAmount = 9.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        DeltaAmountPerDate(deltaAmount = 11.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
        DeltaAmountPerDate(deltaAmount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
      )

      val result = payments.groupDeltasByDate()

      result shouldBe expected
    }
    describe("fills range") {
      it("at start with missing initial value") {
        val amounts = listOf(
          DeltaAmountPerDate(deltaAmount = 6.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          DeltaAmountPerDate(deltaAmount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val expected = listOf(
          DeltaAmountPerDate(deltaAmount = 0.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          DeltaAmountPerDate(deltaAmount = 6.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          DeltaAmountPerDate(deltaAmount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtStart(range)

        result shouldBe expected
      }
      it("at start with existing initial value") {
        val amounts = listOf(
          DeltaAmountPerDate(deltaAmount = 5.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          DeltaAmountPerDate(deltaAmount = 6.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          DeltaAmountPerDate(deltaAmount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val expected = listOf(
          DeltaAmountPerDate(deltaAmount = 5.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          DeltaAmountPerDate(deltaAmount = 6.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          DeltaAmountPerDate(deltaAmount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtStart(range)

        result shouldBe expected
      }
      it("at end with missing end value") {
        val amounts = listOf(
          AmountPerDate(amount = 6.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          AmountPerDate(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val expected = listOf(
          AmountPerDate(amount = 6.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          AmountPerDate(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtEnd(range)

        result shouldBe expected
      }
      it("at end with existing end value") {
        val amounts = listOf(
          AmountPerDate(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
        )
        val expected = listOf(
          AmountPerDate(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          AmountPerDate(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtEnd(range)

        result shouldBe expected
      }
      it("at start with empty list") {
        val amounts = emptyList<DeltaAmountPerDate>()
        val expected = listOf(
          DeltaAmountPerDate(deltaAmount = 0.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtStart(range)

        result shouldBe expected
      }
      it("at end with empty list") {
        val amounts = emptyList<AmountPerDate>()
        val expected = listOf(
          AmountPerDate(amount = 0.toBigDecimal(), date = LocalDate.parse("2021-04-02")),
        )
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-04-01")

        val result = amounts.fillAtEnd(range)

        result shouldBe expected
      }
    }
    it("sums daily amounts") {
      val initialAmount = 10.toBigDecimal()
      val dailyAmounts = listOf(
        DeltaAmountPerDate(deltaAmount = 1.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        DeltaAmountPerDate(deltaAmount = 2.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
        DeltaAmountPerDate(deltaAmount = 3.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
      )
      val expected = listOf(
        AmountPerDate(amount = 11.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
        AmountPerDate(amount = 13.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
        AmountPerDate(amount = 16.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
      )

      val result = dailyAmounts.deltasToAbsolutes(initialAmount)

      result shouldBe expected
    }
    describe("aggregates everything") {
      it("without range boundaries") {
        val payments = listOf(
          Payment(amount = 2.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          Payment(amount = 6.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          Payment(amount = 4.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 5.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 3.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
        )
        val initialAmount = 11.toBigDecimal()
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-05-01")
        val expected = listOf(
          AmountPerDate(amount = 11.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          AmountPerDate(amount = 22.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 35.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          AmountPerDate(amount = 38.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
          AmountPerDate(amount = 38.toBigDecimal(), date = LocalDate.parse("2021-05-01")),
        )

        val result = payments.aggregateAmountsPerDate(initialAmount, range)

        result shouldBe expected
      }
      it("with range boundaries") {
        val payments = listOf(
          Payment(amount = 2.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 7.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          Payment(amount = 6.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          Payment(amount = 1.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          Payment(amount = 4.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 8.toBigDecimal(), date = LocalDate.parse("2021-05-01")),
          Payment(amount = 5.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          Payment(amount = 3.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
        )
        val initialAmount = 11.toBigDecimal()
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-05-01")
        val expected = listOf(
          AmountPerDate(amount = 12.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          AmountPerDate(amount = 23.toBigDecimal(), date = LocalDate.parse("2021-02-01")),
          AmountPerDate(amount = 36.toBigDecimal(), date = LocalDate.parse("2021-03-01")),
          AmountPerDate(amount = 39.toBigDecimal(), date = LocalDate.parse("2021-04-01")),
          AmountPerDate(amount = 47.toBigDecimal(), date = LocalDate.parse("2021-05-01")),
        )

        val result = payments.aggregateAmountsPerDate(initialAmount, range)

        result shouldBe expected
      }
      it("with empty list") {
        val payments = emptyList<Payment>()
        val initialAmount = 11.toBigDecimal()
        val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-05-01")
        val expected = listOf(
          AmountPerDate(amount = 11.toBigDecimal(), date = LocalDate.parse("2021-01-01")),
          AmountPerDate(amount = 11.toBigDecimal(), date = LocalDate.parse("2021-05-01")),
        )

        val result = payments.aggregateAmountsPerDate(initialAmount, range)

        result shouldBe expected
      }
    }
  }
})

private fun List<Payment>.aggregateAmountsPerDate(
  initialAmount: BigDecimal,
  range: ClosedRange<LocalDate>
): List<AmountPerDate> =
  groupDeltasByDate().fillAtStart(range).deltasToAbsolutes(initialAmount).fillAtEnd(range)

private fun List<DeltaAmountPerDate>.fillAtStart(range: ClosedRange<LocalDate>): List<DeltaAmountPerDate> =
  if (isEmpty()) listOf(DeltaAmountPerDate(0.toBigDecimal(), date = range.start))
  else first()
    .takeUnless { it.date == range.start }
    ?.copy(deltaAmount = 0.toBigDecimal(), date = range.start)
    ?.let { listOf(it) + this }
    ?: this

private fun List<AmountPerDate>.fillAtEnd(range: ClosedRange<LocalDate>): List<AmountPerDate> =
  if (isEmpty()) listOf(AmountPerDate(0.toBigDecimal(), date = range.endInclusive))
  else last()
    .takeUnless { it.date == range.endInclusive }
    ?.copy(date = range.endInclusive)
    ?.let { this + it }
    ?: this

private fun List<Payment>.groupDeltasByDate(): List<DeltaAmountPerDate> =
  groupBy { it.date }
    .map { (date, payments) -> DeltaAmountPerDate(payments.sumOf { it.amount }, date) }
    .sortedBy { it.date }

private fun List<DeltaAmountPerDate>.deltasToAbsolutes(initialAmount: BigDecimal): List<AmountPerDate> =
  sequence {
    fold(initialAmount) { lastAmount, current ->
      yield(current.makeAbsolute(lastAmount))
      lastAmount + current.deltaAmount
    }
  }.toList()

private fun DeltaAmountPerDate.makeAbsolute(baseAmount: BigDecimal) =
  AmountPerDate(deltaAmount + baseAmount, date)

