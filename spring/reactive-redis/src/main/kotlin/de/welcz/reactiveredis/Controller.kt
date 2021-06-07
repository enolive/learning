package de.welcz.reactiveredis

import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/test")
class Controller(private val greeter: Greeter) {
  @GetMapping("{name}")
  suspend fun sayHello(@PathVariable name: String): Greeting = greeter.sayHelloTo(name)
}
