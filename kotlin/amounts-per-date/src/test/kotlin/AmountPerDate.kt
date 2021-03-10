import java.math.BigDecimal
import java.time.LocalDate

data class AmountPerDate(
  val amount: BigDecimal,
  val date: LocalDate,
)

data class DeltaAmountPerDate(
  val deltaAmount: BigDecimal,
  val date: LocalDate,
)
