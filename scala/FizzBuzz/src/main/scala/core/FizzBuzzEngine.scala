package core

class FizzBuzzEngine() {
  val rules = Array(
    new Rule(15, "Fizz-Buzz"),
    new Rule(3, "Fizz"),
    new Rule(5, "Buzz")
  )

  def calculate(number: Int): String = {
    val matchingRule = this.rules
      .filter(r => r.appliesTo(number))
      .map(r => r.giveResult())
      .headOption

    matchingRule getOrElse number.toString
  }
}
