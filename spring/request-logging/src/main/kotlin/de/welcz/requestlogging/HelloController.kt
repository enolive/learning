package de.welcz.requestlogging

import mu.KLogging
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RestController

@RestController
class HelloController {
  companion object : KLogging()

  @GetMapping("hellos")
  suspend fun sayHello(@RequestBody(required = false) recipient: HelloRecipient): String {
    logger.info("from controller {}", recipient)
    return "Hello World"
  }
}
