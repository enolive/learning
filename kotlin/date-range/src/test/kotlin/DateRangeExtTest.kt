import io.kotest.core.spec.style.DescribeSpec
import io.kotest.data.forAll
import io.kotest.data.row
import io.kotest.inspectors.forAll
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.collections.shouldContainExactly
import io.kotest.matchers.collections.shouldNotContain
import io.kotest.matchers.collections.shouldStartWith
import io.kotest.matchers.comparables.shouldBeGreaterThanOrEqualTo
import io.kotest.matchers.comparables.shouldBeLessThanOrEqualTo
import io.kotest.property.Arb
import io.kotest.property.arbitrary.*
import io.kotest.property.checkAll
import java.time.LocalDate
import java.time.Period

const val UPPER_BOUND_YEARS = 1_000_000_000

class DateRangeExtTest : DescribeSpec({
  val arbPositive = Arb.nats()
  val arbPositiveInYearBounds = Arb.positiveInts(UPPER_BOUND_YEARS - 1)

  describe("date range operations") {
    val arbDateRange = Arb.pair(Arb.localDate(), Arb.localDate())
    val arbDateRangeWithStartBeforeEnd = arbDateRange.filter { (start, end) -> start <= end }
    val arbStep =
      arbPositive.map(Period::ofDays)
        .merge(arbPositive.map(Period::ofMonths))
        .merge(arbPositiveInYearBounds.map(Period::ofYears))

    describe("generates a sequence up to") {
      it("is empty") {
        val startAfterEnd = arbDateRange.filterNot { (start, end) -> start <= end }
        checkAll(startAfterEnd, arbStep) { (start, end), step ->
          val range = start..end step step

          range.shouldBeEmpty()
        }
      }

      it("does not include values after the end") {
        checkAll(arbDateRangeWithStartBeforeEnd, arbStep) { (start, end), step ->
          val range = start..end step step

          range.asSequence().forAll {
            it shouldBeGreaterThanOrEqualTo start
            it shouldBeLessThanOrEqualTo end
          }
        }
      }

      describe("daily") {
        it("simple") {
          val start = LocalDate.parse("2021-01-01")
          val end = LocalDate.parse("2021-01-07")

          val range = start..end

          range shouldContainExactly listOf(
            LocalDate.parse("2021-01-01"),
            LocalDate.parse("2021-01-02"),
            LocalDate.parse("2021-01-03"),
            LocalDate.parse("2021-01-04"),
            LocalDate.parse("2021-01-05"),
            LocalDate.parse("2021-01-06"),
            LocalDate.parse("2021-01-07"),
          )
        }

        it("for days across months") {
          val start = LocalDate.parse("2020-01-21")
          val end = LocalDate.parse("2020-03-02")
          val expected = listOf(
            "2020-01-21",
            "2020-01-22",
            "2020-01-23",
            "2020-01-24",
            "2020-01-25",
            "2020-01-26",
            "2020-01-27",
            "2020-01-28",
            "2020-01-29",
            "2020-01-30",
            "2020-01-31",
            "2020-02-01",
            "2020-02-02",
            "2020-02-03",
            "2020-02-04",
            "2020-02-05",
            "2020-02-06",
            "2020-02-07",
            "2020-02-08",
            "2020-02-09",
            "2020-02-10",
            "2020-02-11",
            "2020-02-12",
            "2020-02-13",
            "2020-02-14",
            "2020-02-15",
            "2020-02-16",
            "2020-02-17",
            "2020-02-18",
            "2020-02-19",
            "2020-02-20",
            "2020-02-21",
            "2020-02-22",
            "2020-02-23",
            "2020-02-24",
            "2020-02-25",
            "2020-02-26",
            "2020-02-27",
            "2020-02-28",
            "2020-02-29",
            "2020-03-01",
            "2020-03-02",
          ).map { LocalDate.parse(it) }

          val range = start..end

          range shouldContainExactly expected
        }

        it("for one week") {
          val start = LocalDate.parse("2021-01-01")
          val end = LocalDate.parse("2021-01-17")

          val range = start..end step Period.ofWeeks(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-01-01"),
            LocalDate.parse("2021-01-08"),
            LocalDate.parse("2021-01-15"),
          )
        }
      }

      describe("monthly") {
        it("simple") {
          val start = LocalDate.parse("2021-01-01")
          val end = LocalDate.parse("2021-05-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-01-01"),
            LocalDate.parse("2021-02-01"),
            LocalDate.parse("2021-03-01"),
            LocalDate.parse("2021-04-01"),
            LocalDate.parse("2021-05-01"),
          )
        }

        it("for end of month") {
          val start = LocalDate.parse("2021-01-31")
          val end = LocalDate.parse("2021-05-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-01-31"),
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2021-03-31"),
            LocalDate.parse("2021-04-30"),
            LocalDate.parse("2021-05-31"),
          )
        }

        it("starting at end of month with 30 days") {
          val start = LocalDate.parse("2021-11-30")
          val end = LocalDate.parse("2022-03-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-11-30"),
            LocalDate.parse("2021-12-30"),
            LocalDate.parse("2022-01-30"),
            LocalDate.parse("2022-02-28"),
            LocalDate.parse("2022-03-30"),
          )
        }

        it("for end of month in leap year") {
          val start = LocalDate.parse("2020-01-31")
          val end = LocalDate.parse("2020-05-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2020-01-31"),
            LocalDate.parse("2020-02-29"),
            LocalDate.parse("2020-03-31"),
            LocalDate.parse("2020-04-30"),
            LocalDate.parse("2020-05-31"),
          )
        }

        it("for end of february in leap year") {
          val start = LocalDate.parse("2020-02-29")
          val end = LocalDate.parse("2020-05-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2020-02-29"),
            LocalDate.parse("2020-03-29"),
            LocalDate.parse("2020-04-29"),
            LocalDate.parse("2020-05-29"),
          )
        }

        it("for end of february not in leap year") {
          val start = LocalDate.parse("2021-02-28")
          val end = LocalDate.parse("2021-05-31")

          val range = start..end step Period.ofMonths(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2021-03-28"),
            LocalDate.parse("2021-04-28"),
            LocalDate.parse("2021-05-28"),
          )
        }

        describe("for >= 28th of month that is not february") {
          forAll(
            row(
              LocalDate.parse("2021-03-28"), LocalDate.parse("2021-05-31"),
              listOf(
                LocalDate.parse("2021-03-28"),
                LocalDate.parse("2021-04-28"),
                LocalDate.parse("2021-05-28"),
              )
            ),
            row(
              LocalDate.parse("2021-01-29"), LocalDate.parse("2021-06-30"),
              listOf(
                LocalDate.parse("2021-01-29"),
                LocalDate.parse("2021-02-28"),
                LocalDate.parse("2021-03-29"),
                LocalDate.parse("2021-04-29"),
                LocalDate.parse("2021-05-29"),
                LocalDate.parse("2021-06-29"),
              )
            ),
            row(
              LocalDate.parse("2021-01-30"), LocalDate.parse("2021-05-31"),
              listOf(
                LocalDate.parse("2021-01-30"),
                LocalDate.parse("2021-02-28"),
                LocalDate.parse("2021-03-30"),
                LocalDate.parse("2021-04-30"),
                LocalDate.parse("2021-05-30"),
              )
            ),
          ) { start, end, expected ->
            it("$start") {
              val range = start..end step Period.ofMonths(1)

              range shouldContainExactly expected
            }
          }
        }
      }

      it("for 29th january not in leap year") {
        val start = LocalDate.parse("2021-01-29")
        val end = LocalDate.parse("2021-05-31")

        val range = start..end step Period.ofMonths(1)

        range shouldContainExactly listOf(
          LocalDate.parse("2021-01-29"),
          LocalDate.parse("2021-02-28"),
          LocalDate.parse("2021-03-29"),
          LocalDate.parse("2021-04-29"),
          LocalDate.parse("2021-05-29"),
        )
      }

      describe("Yearly") {
        it("for year") {
          val start = LocalDate.parse("2020-01-01")
          val end = LocalDate.parse("2025-12-31")

          val range = start..end step Period.ofYears(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2020-01-01"),
            LocalDate.parse("2021-01-01"),
            LocalDate.parse("2022-01-01"),
            LocalDate.parse("2023-01-01"),
            LocalDate.parse("2024-01-01"),
            LocalDate.parse("2025-01-01"),
          )
        }

        it("for year at end of month in february in leap year") {
          val start = LocalDate.parse("2020-02-29")
          val end = LocalDate.parse("2025-12-31")

          val range = start..end step Period.ofYears(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2020-02-29"),
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2022-02-28"),
            LocalDate.parse("2023-02-28"),
            LocalDate.parse("2024-02-29"),
            LocalDate.parse("2025-02-28"),
          )
        }

        it("for year at end of month in february not in leap year") {
          val start = LocalDate.parse("2021-02-28")
          val end = LocalDate.parse("2025-12-31")

          val range = start..end step Period.ofYears(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2022-02-28"),
            LocalDate.parse("2023-02-28"),
            LocalDate.parse("2024-02-28"),
            LocalDate.parse("2025-02-28"),
          )
        }

        it("for year at the middle of month") {
          val start = LocalDate.parse("2021-02-15")
          val end = LocalDate.parse("2025-12-31")

          val range = start..end step Period.ofYears(1)

          range shouldContainExactly listOf(
            LocalDate.parse("2021-02-15"),
            LocalDate.parse("2022-02-15"),
            LocalDate.parse("2023-02-15"),
            LocalDate.parse("2024-02-15"),
            LocalDate.parse("2025-02-15"),
          )
        }
      }
    }

    describe("generate a sequence until") {
      it("end is exclusive") {
        checkAll(arbDateRangeWithStartBeforeEnd, arbStep) { (start, end), step ->
          val range = start until end step step
          val inclusiveRange = start..end step step

          range shouldNotContain end
          inclusiveRange shouldStartWith range
        }
      }
    }
  }
})
