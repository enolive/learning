package de.welcz.hello;

import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

class GreetingWebClient {
    private WebClient client = WebClient.create("http://localhost:8080");

    private Mono<ClientResponse> result = client.get()
                                                .uri("/hello")
                                                .accept(MediaType.TEXT_PLAIN)
                                                .exchange();

    String getResult() {
        return ">> result = " + result.flatMap(response -> response.bodyToMono(String.class)).block();
    }
}
