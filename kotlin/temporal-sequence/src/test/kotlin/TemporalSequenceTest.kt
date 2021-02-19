import io.kotest.core.spec.style.DescribeSpec
import io.kotest.inspectors.forAll
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.collections.shouldContainExactly
import io.kotest.matchers.collections.shouldNotBeEmpty
import io.kotest.matchers.comparables.shouldBeLessThanOrEqualTo
import io.kotest.property.Arb
import io.kotest.property.Exhaustive
import io.kotest.property.arbitrary.bind
import io.kotest.property.arbitrary.filter
import io.kotest.property.arbitrary.localDate
import io.kotest.property.arbitrary.nats
import io.kotest.property.checkAll
import io.kotest.property.exhaustive.of
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.time.temporal.ChronoUnit.*

class TemporalSequenceTest : DescribeSpec({
  describe("generate temporal sequence") {
    val dateAndRange = Arb.bind(Arb.localDate(), Arb.localDate(), Arb.localDate()) { date, start, end ->
      date to start..end
    }
    val notEmptyRange = dateAndRange.filter { (_, range) -> range.start < range.endInclusive }
    val supportedUnits = Exhaustive.of(DAYS, WEEKS, MONTHS, YEARS)
    val widths = Arb.nats(max = 10000)
    val steps = Arb.bind(widths, supportedUnits, ::Step)

    it("returns empty for start after range") {
      val startAfterRange = dateAndRange.filter { (start, range) -> start > range.endInclusive }
      checkAll(startAfterRange, steps) { (start, range), step ->
        range.temporalSequence(step, start).shouldBeEmpty()
      }
    }

    it("returns empty for end before range") {
      val endBeforeRange = dateAndRange.filter { (end, range) -> end < range.start }
      checkAll(endBeforeRange, steps) { (end, range), step ->
        range.temporalSequence(step, range.start, end).shouldBeEmpty()
      }
    }

    it("supports a pre-emptive end") {
      val endBeforeRangeEnd = dateAndRange.filter { (end, range) -> range.contains(end) && end < range.endInclusive }
      checkAll(endBeforeRangeEnd, steps) { (end, range), step ->
        range.temporalSequence(step, range.start, end).forAll {
          it shouldBeLessThanOrEqualTo end
        }
      }
    }

    it("returns non-empty ordered list for not-empty ranges") {
      checkAll(notEmptyRange, steps) { (_, range), step ->
        range.temporalSequence(step, range.start, range.endInclusive).shouldNotBeEmpty()
      }
    }

    describe("examples") {
      describe("daily") {
        val daily = Step(1, DAYS)
        it("start before beginning") {
          val start = LocalDate.parse("2021-01-01")
          val range = LocalDate.parse("2021-01-15")..LocalDate.parse("2021-01-20")

          val result = range.temporalSequence(daily, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-01-15"),
            LocalDate.parse("2021-01-16"),
            LocalDate.parse("2021-01-17"),
            LocalDate.parse("2021-01-18"),
            LocalDate.parse("2021-01-19"),
            LocalDate.parse("2021-01-20"),
          )
        }
        it("start in range") {
          val start = LocalDate.parse("2021-01-17")
          val range = LocalDate.parse("2021-01-15")..LocalDate.parse("2021-01-20")

          val result = range.temporalSequence(daily, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-01-17"),
            LocalDate.parse("2021-01-18"),
            LocalDate.parse("2021-01-19"),
            LocalDate.parse("2021-01-20"),
          )
        }
      }
      describe("every 10th day") {
        val tenDays = Step(10, DAYS)
        it("start before beginning") {
          val start = LocalDate.parse("2021-01-06")
          val range = LocalDate.parse("2021-01-15")..LocalDate.parse("2021-01-20")

          val result = range.temporalSequence(tenDays, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-01-16"),
          )
        }
        it("start in range") {
          val start = LocalDate.parse("2021-01-16")
          val range = LocalDate.parse("2021-01-15")..LocalDate.parse("2021-01-31")

          val result = range.temporalSequence(tenDays, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-01-16"),
            LocalDate.parse("2021-01-26"),
          )
        }
      }
      describe("monthly") {
        val monthly = Step(1, MONTHS)
        it("start before range") {
          val start = LocalDate.parse("2019-08-31")
          val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2021-06-01")

          val result = range.temporalSequence(monthly, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-01-31"),
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2021-03-31"),
            LocalDate.parse("2021-04-30"),
            LocalDate.parse("2021-05-31"),
          )
        }
        it("start in range") {
          val start = LocalDate.parse("2020-03-31")
          val range = LocalDate.parse("2020-01-01")..LocalDate.parse("2020-06-01")

          val result = range.temporalSequence(monthly, start)

          result.shouldContainExactly(
            LocalDate.parse("2020-03-31"),
            LocalDate.parse("2020-04-30"),
            LocalDate.parse("2020-05-31"),
          )
        }
      }
      describe("weekly") {
        val weekly = Step(1, WEEKS)
        it("start before range") {
          val start = LocalDate.parse("2021-01-18")
          val range = LocalDate.parse("2021-02-01")..LocalDate.parse("2021-03-01")

          val result = range.temporalSequence(weekly, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-02-01"),
            LocalDate.parse("2021-02-08"),
            LocalDate.parse("2021-02-15"),
            LocalDate.parse("2021-02-22"),
            LocalDate.parse("2021-03-01"),
          )
        }
        it("start in range") {
          val start = LocalDate.parse("2020-03-31")
          val range = LocalDate.parse("2020-01-01")..LocalDate.parse("2020-05-01")

          val result = range.temporalSequence(weekly, start)

          result.shouldContainExactly(
            LocalDate.parse("2020-03-31"),
            LocalDate.parse("2020-04-07"),
            LocalDate.parse("2020-04-14"),
            LocalDate.parse("2020-04-21"),
            LocalDate.parse("2020-04-28"),
          )
        }
      }
      describe("yearly") {
        val yearly = Step(1, YEARS)
        it("start before range") {
          val start = LocalDate.parse("2019-02-01")
          val range = LocalDate.parse("2021-01-01")..LocalDate.parse("2024-12-31")

          val result = range.temporalSequence(yearly, start)

          result.shouldContainExactly(
            LocalDate.parse("2021-02-01"),
            LocalDate.parse("2022-02-01"),
            LocalDate.parse("2023-02-01"),
            LocalDate.parse("2024-02-01"),
          )
        }
        it("start in range") {
          val start = LocalDate.parse("2020-02-29")
          val range = LocalDate.parse("2019-01-01")..LocalDate.parse("2024-12-31")

          val result = range.temporalSequence(yearly, start)

          result.shouldContainExactly(
            LocalDate.parse("2020-02-29"),
            LocalDate.parse("2021-02-28"),
            LocalDate.parse("2022-02-28"),
            LocalDate.parse("2023-02-28"),
            LocalDate.parse("2024-02-29"),
          )
        }
      }
    }
  }
})

data class Step(val width: Int, val unit: ChronoUnit)

fun ClosedRange<LocalDate>.temporalSequence(step: Step, from: LocalDate, to: LocalDate? = null): List<LocalDate> {
  val stepsUntilStart = countNumberOfSteps(from, start, step)
  val startingPoint = from.times(stepsUntilStart, step)
  val endingPoint = to?.let { minOf(it, endInclusive) } ?: endInclusive
  val stepsUntilEnd = countNumberOfSteps(startingPoint, endingPoint.plusDays(1), step)
  return (0 until stepsUntilEnd).map { startingPoint.times(it, step) }
}

private fun LocalDate.times(times: Int, step: Step) =
  plus(times * step.width.toLong(), step.unit)

fun countNumberOfSteps(
  start: LocalDate,
  goal: LocalDate,
  step: Step,
): Int {
  tailrec fun stepsRec(current: LocalDate, steps: Int): Int = when {
    current >= goal -> steps
    else            -> stepsRec(current.plus(step.width.toLong(), step.unit), steps + 1)
  }
  return stepsRec(start, 0)
}

