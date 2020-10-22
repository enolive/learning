package de.welcz.emojify.log

import mu.KLogging
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  companion object : KLogging()

  @Bean
  fun run() = CommandLineRunner {
    logger.info("This is an info")
    logger.warn("This is a warning")
    logger.debug("This is a debug")
    logger.trace("This is a trace")
    logger.error("This is an error", RuntimeException("I AM ERROR"))
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
