class Greeter() {
    def sayHelloTo(name: String): String = {
        println("was called")
        s"Hello, $name!"
    }

    def sayHello(): String = sayHelloTo("World")
}
