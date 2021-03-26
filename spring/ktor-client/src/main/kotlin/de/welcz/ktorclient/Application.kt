package de.welcz.ktorclient

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class KtorClientApplication

fun main(args: Array<String>) {
  runApplication<KtorClientApplication>(*args)
}
