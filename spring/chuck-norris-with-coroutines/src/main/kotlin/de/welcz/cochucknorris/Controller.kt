package de.welcz.cochucknorris

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController

@RestController
class Controller(private val fetchJoke: FetchJoke) {
  @GetMapping("/jokes/random")
  suspend fun randomJoke() =
      fetchJoke
        .random()
        .let(::JokeResponse)
}
