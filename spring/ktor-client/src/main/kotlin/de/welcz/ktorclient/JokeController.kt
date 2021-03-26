package de.welcz.ktorclient

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class JokeController(private val api: JokeApi) {

  @GetMapping("jokes/random")
  suspend fun getRandomJoke(): JokeResponse = api.fetchRandomJoke()
}
