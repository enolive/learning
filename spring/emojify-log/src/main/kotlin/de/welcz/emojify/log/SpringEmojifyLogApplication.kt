package de.welcz.emojify.log

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class SpringEmojifyLogApplication

fun main(args: Array<String>) {
  runApplication<SpringEmojifyLogApplication>(*args)
}
