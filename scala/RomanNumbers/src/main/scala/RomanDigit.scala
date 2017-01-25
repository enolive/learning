
class RomanDigit(val digit: Char) {

  def toArabic: Int = {
    digit match {
            case 'I' => 1
            case 'V' => 5
            case 'X' => 10
            case 'L' => 50
            case 'C' => 100
            case 'D' => 500
            case 'M' => 1000
        }
    }

  def canEqual(other: Any): Boolean = other.isInstanceOf[RomanDigit]

  override def equals(other: Any): Boolean = other match {
    case that: RomanDigit =>
      (that canEqual this) &&
        digit == that.digit
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(digit)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
