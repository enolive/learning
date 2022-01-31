package de.welcz.rabbitmq

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import java.util.function.Consumer
import java.util.function.Function


@SpringBootApplication
class Application {
//  @Bean
//  fun log(): Consumer<Person> {
//    return Consumer<Person> { person -> println("Received: $person") }
//  }

  @Bean("uppercase-consume")
  fun testUpperCase(): Consumer<String> {
    return Consumer { input -> println("Received: $input") }
  }

  @Bean("uppercase")
  fun upperCase(): Function<String, String> {
    return Function { input ->
      println("Received: $input")
      input.uppercase()
    }
  }

  @Bean
  fun reverse(): Function<String, String> {
    return Function { input ->
      println("Received: $input")
      input.reversed()
    }
  }

  data class Person(
    val name: String,
  )
}


fun main(args: Array<String>) {
  runApplication<Application>(*args)
}

