package de.welcz.reactiveredis

import mu.KLogging
import org.springframework.cache.annotation.Cacheable
import org.springframework.stereotype.Service

@Service
class Greeter {
  companion object : KLogging()

  @Cacheable("greetings")
  fun sayHelloTo(name: String): Greeting {
    logger.info { "calculating for $name..." }
    return Greeting(name)
  }
}
