package de.welcz.cochucknorris

import org.springframework.stereotype.Service
import org.springframework.web.reactive.function.client.WebClient
import org.springframework.web.reactive.function.client.awaitBody

@Service
class FetchJoke(private val builder: WebClient.Builder,
                private val config: FetchConfig) {
  suspend fun random() =
      builder
        .baseUrl(config.host)
        .build()
        .get()
        .uri("/jokes/random?category=dev")
        .retrieve()
        .awaitBody<ResponseValue>()
        .value

  data class ResponseValue(val value: String)
}
