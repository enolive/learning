package core

class Rule(denominator: Int, result: String) {

  def giveResult(): String = {
    result
  }

  def appliesTo(number: Int): Boolean = {
    isDivisibleBy(number, denominator)
  }

  private def isDivisibleBy(number: Int, denominator: Int): Boolean = {
    number % denominator == 0
  }
}
