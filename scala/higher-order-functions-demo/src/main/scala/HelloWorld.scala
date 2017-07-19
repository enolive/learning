object HelloWorld extends App {
    val greeter = new Greeter()

    // the old style
    // Logger.log(greeter.sayHello())

    // the new style
    // Logger.log(() => greeter.sayHello())
}
