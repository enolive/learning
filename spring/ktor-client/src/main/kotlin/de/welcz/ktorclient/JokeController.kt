package de.welcz.ktorclient

import io.ktor.client.*
import io.ktor.client.request.*
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class JokeController(private val httpClient: HttpClient) {
  data class JokeResponse(val value: String)

  @GetMapping("jokes/random")
  suspend fun getRandomJoke(): JokeResponse = httpClient.get("https://api.chucknorris.io/jokes/random")
}
