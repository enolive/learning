package de.welcz.webclientwithmockserver

import com.ninjasquad.springmockk.MockkBean
import io.kotest.assertions.json.shouldMatchJson
import io.kotest.core.spec.style.DescribeSpec
import io.mockk.coEvery
import org.intellij.lang.annotations.Language
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.test.web.reactive.server.expectBody

@WebFluxTest(controllers = [JokeController::class])
class JokeControllerTest(
  private val webTestClient: WebTestClient,
  @MockkBean private val client: JokeClient,
) : DescribeSpec({
  describe("API for /jokes") {
    it("has GET /random") {
      coEvery { client.fetchRandomJoke() } returns JokeResponse(value = "Test Joke")
      @Language("JSON")
      val expectedJson = """{"value":  "Test Joke"}"""

      val response = webTestClient.get().uri("/jokes/random").exchange()

      response.expectStatus().isOk
      response.expectBody<String>().consumeWith {
        it.responseBody shouldMatchJson expectedJson
      }
    }
  }
})


