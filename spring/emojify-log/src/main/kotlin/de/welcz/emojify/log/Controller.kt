package de.welcz.emojify.log

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class Controller {
  companion object : WithLogging()

  @GetMapping("/hello")
  fun sayHello(): String {
    logger.info("Test")
    logger.error("Nooo!", RuntimeException("I AM ERROR"))
    return "Hello, World!"
  }
}

