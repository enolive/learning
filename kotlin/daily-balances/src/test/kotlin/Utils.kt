import java.time.LocalDate

fun wholeDateRange(
  min: LocalDate,
  max: LocalDate,
): List<LocalDate> = min.datesUntil(max.plusDays(1)).toList()
