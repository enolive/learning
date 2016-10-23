package core.test

import core.FizzBuzzEngine
import org.scalatest.{FlatSpec, Matchers}

class FizzBuzzSpec extends FlatSpec with Matchers {
  val target = FizzBuzzEngine()

  behavior of "Fizz Buzz Engine"

  it should "return normal numbers as is" in {
    this.target.calculate(1) should be("1")
    this.target.calculate(2) should be("2")
    this.target.calculate(4) should be("4")
  }

  it should "return numbers divisible by 3 as Fizz" in {
    this.target.calculate(3) should be("Fizz")
    this.target.calculate(6) should be("Fizz")
    this.target.calculate(9) should be("Fizz")
  }

  it should "return numbers divisible by 5 as Buzz" in {
    this.target.calculate(5) should be("Buzz")
    this.target.calculate(10) should be("Buzz")
  }

  it should "return numbers divisible by both 4 and 5 as Fizz-Buzz" in {
    this.target.calculate(15) should be("Fizz-Buzz")
    this.target.calculate(30) should be("Fizz-Buzz")
  }
}
