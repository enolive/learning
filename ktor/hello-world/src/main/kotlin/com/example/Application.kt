package com.example

import io.ktor.application.*
import io.ktor.features.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.serialization.*

fun main(args: Array<String>): Unit =
  io.ktor.server.netty.EngineMain.main(args)

/**
 * Please note that you can use any other name instead of *module*.
 * Also note that you can have more then one modules in your application.
 * */
@Suppress("unused") // Referenced in application.conf
fun Application.module(testing: Boolean = false) {
  install(ContentNegotiation) {
    json()
  }
  routing {
    get("/") {
      call.respond(Greeting("Hello, World!"))
    }
  }
}
