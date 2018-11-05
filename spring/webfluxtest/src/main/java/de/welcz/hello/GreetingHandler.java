package de.welcz.hello;

import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import static java.text.MessageFormat.format;
import static org.springframework.web.reactive.function.BodyInserters.fromObject;
import static org.springframework.web.reactive.function.server.ServerResponse.ok;

@Component
public class GreetingHandler {
    Mono<ServerResponse> hello(ServerRequest request) {
        final var name = request.pathVariables().getOrDefault("name", "World");
        final var response = format("Hello, {0}! Guess what''s the best programming language in the world...", name);
        return ok().contentType(MediaType.TEXT_PLAIN)
                   .body(fromObject(response));
    }
}
