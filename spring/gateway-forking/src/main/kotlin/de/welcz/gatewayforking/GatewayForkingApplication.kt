package de.welcz.gatewayforking

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class GatewayForkingApplication

fun main(args: Array<String>) {
  runApplication<GatewayForkingApplication>(*args)
}
