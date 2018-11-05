package de.welcz.hello

import org.springframework.http.MediaType
import org.springframework.web.reactive.function.client.ClientResponse
import org.springframework.web.reactive.function.client.WebClient
import reactor.core.publisher.Mono

internal class GreetingWebClient {
    private val client = WebClient.create("http://localhost:8080")

    private val result = client.get()
            .uri("/hello")
            .accept(MediaType.TEXT_PLAIN)
            .exchange()

    fun retrieveResult(): String {
        return ">> result = " + result.flatMap { response -> response.bodyToMono(String::class.java) }.block()!!
    }
}
