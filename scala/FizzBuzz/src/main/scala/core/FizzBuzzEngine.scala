package core

case class FizzBuzzEngine() {
  val rules = Array(
    Rule(15, "Fizz-Buzz"),
    Rule(3, "Fizz"),
    Rule(5, "Buzz")
  )

  def calculate(number: Int): String = {
    val matchingRule = this.rules
      .filter(r => r.appliesTo(number))
      .map(r => r.result)
      .headOption

    matchingRule getOrElse number.toString
  }
}
