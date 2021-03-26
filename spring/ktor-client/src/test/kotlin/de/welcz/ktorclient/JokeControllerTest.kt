package de.welcz.ktorclient

import com.ninjasquad.springmockk.MockkBean
import io.kotest.assertions.json.shouldMatchJson
import io.kotest.core.spec.style.DescribeSpec
import io.mockk.coEvery
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.test.web.reactive.server.expectBody

@WebFluxTest(controllers = [JokeController::class])
class JokeControllerTest(
  private val webTestClient: WebTestClient,
  @MockkBean private val api: JokeApi,
) : DescribeSpec({
  describe("API for /jokes") {
    it("has GET /random") {
      coEvery { api.fetchRandomJoke() } returns JokeResponse("Test Joke")

      val response = webTestClient.get().uri("/jokes/random").exchange()

      response.expectStatus().isOk
      response.expectBody<String>().consumeWith {
        it.responseBody shouldMatchJson """{"value": "Test Joke"}"""
      }
    }
  }
})

