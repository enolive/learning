package de.welcz.hello

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
open class Application

fun main(args: Array<String>) {
    runApplication<Application>(*args)

    val client = GreetingWebClient()
    println(client.retrieveResult())
}
