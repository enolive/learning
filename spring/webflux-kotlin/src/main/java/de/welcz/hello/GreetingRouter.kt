package de.welcz.hello

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.http.MediaType
import org.springframework.web.reactive.function.server.router

@Configuration
open class GreetingRouter(private val handler: GreetingHandler) {

    @Bean
    open fun composedRoutes() = router {
        accept(MediaType.TEXT_PLAIN).nest {
            (GET("/hello") or GET("/hello/{name}")).invoke(handler::hello)
        }
    }
}
