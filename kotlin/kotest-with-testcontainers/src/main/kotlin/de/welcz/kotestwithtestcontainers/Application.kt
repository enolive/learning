package de.welcz.kotestwithtestcontainers

import kotlinx.coroutines.runBlocking
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean

@SpringBootApplication
class Application {
  @Bean
  fun doSomethingWithDb(repository: BeerRepository) = CommandLineRunner {
    runBlocking {
      val beer = Beer(
        id = null,
        brand = "Test",
        name = "Test 2",
        strength = 1.5.toBigDecimal(),
      )
      repository.save(beer)
    }
  }
}

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}
