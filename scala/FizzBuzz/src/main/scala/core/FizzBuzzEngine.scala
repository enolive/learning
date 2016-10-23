package core

case class FizzBuzzEngine() {
  val rules = Array(
    (15, "Fizz-Buzz"),
    (3, "Fizz"),
    (5, "Buzz")
  )

  def calculate(number: Int): String = {
    val matchingRule = this.rules
      .filter { case (denominator, _) => isDivisibleBy(number, denominator) }
      .map { case (_, result) => result }
      .headOption

    matchingRule getOrElse number.toString
  }

  def isDivisibleBy(number: Int, denominator: Int): Boolean = {
    number % denominator == 0
  }
}
