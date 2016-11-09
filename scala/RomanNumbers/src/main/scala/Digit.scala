
class Digit(digit: String) {
  def toArabic: Int = {
    digit match {
      case "I" => 1
      case "V" => 5
      case "X" => 10
      case "L" => 50
      case "C" => 100
      case "D" => 500
      case "M" => 1000
    }
  }
}
