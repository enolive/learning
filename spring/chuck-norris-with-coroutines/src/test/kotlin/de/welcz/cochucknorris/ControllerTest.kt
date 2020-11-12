package de.welcz.cochucknorris

import com.ninjasquad.springmockk.MockkBean
import io.kotest.assertions.asClue
import io.kotest.core.spec.style.FunSpec
import io.mockk.coEvery
import io.mockk.coVerify
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.test.web.reactive.server.expectBody

@WebFluxTest(controllers = [Controller::class])
class ControllerTest(
    private val webTestClient: WebTestClient,
    @MockkBean
    private val fetchJoke: FetchJoke
) : FunSpec({
  test("fetches random joke") {
    val expected = JokeResponse("this is my awesome joke")
    coEvery { fetchJoke.random() } returns "this is my awesome joke"

    val response = webTestClient.get()
      .uri("/jokes/random")
      .exchange()

    response.asClue {
      it.expectStatus().isOk
      it.expectBody<JokeResponse>().isEqualTo(expected)
    }
    coVerify(exactly = 1) { fetchJoke.random() }
  }
})