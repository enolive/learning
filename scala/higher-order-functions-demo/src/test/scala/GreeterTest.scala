import org.scalatest.{FlatSpec, Matchers}

class GreeterTest extends FlatSpec with Matchers {
    val greeter = new Greeter()

    behavior of "Greeter"
    
    it should "greet the world" in {
        greeter.sayHello() should be("Hello, World!")
    }

    it should "greet a person" in {
        greeter.sayHelloTo("Christoph") should be("Hello, Christoph!")
        greeter.sayHelloTo("Tofu") should be("Hello, Tofu!")
    }
}

