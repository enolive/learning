package de.welcz.kotlinxserialization

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.web.reactive.function.server.ServerResponse
import org.springframework.web.reactive.function.server.awaitBody
import org.springframework.web.reactive.function.server.bodyValueAndAwait
import org.springframework.web.reactive.function.server.coRouter
import java.net.URI

@Configuration
class Router {
  @Bean
  fun routes() = coRouter {
    "/router/users".nest {
      GET("") {
        val user = User("Chris", "Welcz")
        ServerResponse.ok().bodyValueAndAwait(user)
      }
      POST("") { req ->
        val user = req.awaitBody<User>()
        ServerResponse.created(URI("/router/users")).bodyValueAndAwait(user)
      }
    }
  }
}