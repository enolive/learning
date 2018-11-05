package de.welcz.hello

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.web.reactive.function.server.HandlerFunction
import org.springframework.web.reactive.function.server.RouterFunction
import org.springframework.web.reactive.function.server.ServerResponse

import org.springframework.web.reactive.function.server.RequestPredicates.GET
import org.springframework.web.reactive.function.server.RouterFunctions.route

@Configuration
open class GreetingRouter(private val handler: GreetingHandler) {

    @Bean
    open fun composedRoutes(): RouterFunction<ServerResponse> {
        return route<ServerResponse>(GET("/hello"), HandlerFunction<ServerResponse> { handler.hello(it) })
                .and(route(GET("/hello/{name}"), HandlerFunction<ServerResponse> { handler.hello(it) }))
    }
}
