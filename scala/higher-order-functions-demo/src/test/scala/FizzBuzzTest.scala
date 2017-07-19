import org.scalatest.{FlatSpec, Matchers}

class FizzBuzzTest extends FlatSpec with Matchers {
    val generator = new FizzBuzzGenerator()

    behavior of "Fizz-Buzz"

    it should "return normal numbers as is" in {
        generator.nextNumber(1) should be("1")
        generator.nextNumber(2) should be("2")
    }

    it should "return numbers divisible by 3 as Fizz" in {
        generator.nextNumber(3) should be("Fizz")
        generator.nextNumber(6) should be("Fizz")
    }

    it should "return numbers divisible by 5 as Buzz" in {
        generator.nextNumber(5) should be("Buzz")
        generator.nextNumber(10) should be("Buzz")
    }

    it should "return numbers divisible by 3 and 5 as Fizz-Buzz" in {
        generator.nextNumber(15) should be("Fizz-Buzz")
        generator.nextNumber(30) should be("Fizz-Buzz")
    }

}