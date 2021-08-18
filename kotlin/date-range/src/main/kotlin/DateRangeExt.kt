import java.time.LocalDate
import java.time.Period

operator fun LocalDate.rangeTo(other: LocalDate) = LocalDateRange(this, other.plusDays(1), Period.ofDays(1))

infix fun LocalDate.until(to: LocalDate) = LocalDateRange(this, to, Period.ofDays(1))

data class LocalDateRange(val start: LocalDate, val endExclusive: LocalDate, val step: Period) : Iterable<LocalDate> {
  override fun iterator(): Iterator<LocalDate> = when {
    start >= endExclusive -> iterator { }
    else                  -> start.datesUntil(endExclusive, step).iterator()
  }

  infix fun step(step: Period): LocalDateRange = copy(step = step)
}
