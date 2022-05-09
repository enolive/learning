import java.math.BigDecimal
import java.time.LocalDate
import java.time.temporal.ChronoUnit

data class Balance(val date: LocalDate, val amount: BigDecimal)

fun List<Balance>.expandToDaily(): List<Balance> {
  if (isEmpty()) {
    return this
  }

  val balancesSorted = sortedBy { it.date }
  return balancesSorted
    .zipWithNext(::fillDayGapsUpTo)
    .flatten() + balancesSorted.last()
}

private fun fillDayGapsUpTo(current: Balance, next: Balance): List<Balance> {
  val daysBetween = current.date.until(next.date, ChronoUnit.DAYS)
  return when (daysBetween) {
    0L   -> listOf(current)
    else -> current.date.datesUntil(next.date).map { current.copy(date = it) }.toList()
  }
}
