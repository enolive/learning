package de.welcz.hello

import org.springframework.http.MediaType
import org.springframework.stereotype.Component
import org.springframework.web.reactive.function.server.ServerRequest
import org.springframework.web.reactive.function.server.ServerResponse
import reactor.core.publisher.Mono

import java.text.MessageFormat.format
import org.springframework.web.reactive.function.BodyInserters.fromObject
import org.springframework.web.reactive.function.server.ServerResponse.ok

@Component
class GreetingHandler {
    internal fun hello(request: ServerRequest): Mono<ServerResponse> {
        val name = request.pathVariables().getOrDefault("name", "World")
        val response = format("Hello, {0}! Guess what''s the best programming language in the world...", name)
        return ok().contentType(MediaType.TEXT_PLAIN)
                .body(fromObject(response))
    }
}
