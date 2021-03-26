package de.welcz.webclientwithmockserver

import org.springframework.stereotype.Service
import org.springframework.web.reactive.function.client.WebClient
import org.springframework.web.reactive.function.client.awaitBody

@Service
class JokeClient(private val builder: WebClient.Builder) {
  internal var baseUrl: String = "https://api.chucknorris.io"

  suspend fun fetchRandomJoke(): JokeResponse = builder
    .baseUrl(baseUrl)
    .build()
    .get()
    .uri("jokes/random")
    .retrieve()
    .awaitBody()
}
