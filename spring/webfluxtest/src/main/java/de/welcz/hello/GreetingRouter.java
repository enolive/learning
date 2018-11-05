package de.welcz.hello;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;

import static org.springframework.web.reactive.function.server.RequestPredicates.GET;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@Configuration
public class GreetingRouter {
    private final GreetingHandler handler;

    public GreetingRouter(GreetingHandler handler) {
        this.handler = handler;
    }

    @Bean
    RouterFunction<ServerResponse> composedRoutes() {
        return route(GET("/hello"), handler::hello)
                .and(route(GET("/hello/{name}"), handler::hello));
    }
}
