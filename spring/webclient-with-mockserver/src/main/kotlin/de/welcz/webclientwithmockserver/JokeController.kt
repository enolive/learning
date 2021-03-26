package de.welcz.webclientwithmockserver

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("jokes")
class JokeController(private val client: JokeClient) {
  @GetMapping("random")
  suspend fun getRandomJoke() = client.fetchRandomJoke()
}
