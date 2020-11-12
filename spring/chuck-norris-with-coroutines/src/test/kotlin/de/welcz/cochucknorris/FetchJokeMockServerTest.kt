package de.welcz.cochucknorris

import com.ninjasquad.springmockk.MockkBean
import io.kotest.core.spec.style.FunSpec
import io.kotest.extensions.mockserver.MockServerListener
import io.kotest.matchers.shouldBe
import io.mockk.every
import org.intellij.lang.annotations.Language
import org.mockserver.client.MockServerClient
import org.mockserver.model.HttpRequest
import org.mockserver.model.HttpResponse
import org.mockserver.model.MediaType
import org.springframework.boot.test.autoconfigure.web.client.AutoConfigureWebClient
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ContextConfiguration
import org.springframework.web.reactive.config.EnableWebFlux

@SpringBootTest
@EnableWebFlux
@AutoConfigureWebClient
@ContextConfiguration(classes = [FetchJoke::class])
class FetchJokeMockServerTest(
    private val sut: FetchJoke,
    @MockkBean
    private val config: FetchConfig
) : FunSpec({
  listener(MockServerListener(1080))

  beforeTest {
    @Language("JSON")
    val json = """{
      "categories": [
        "dev"
      ],
      "created_at": "2020-01-05 13:42:19.324003",
      "icon_url": "https://assets.chucknorris.host/img/avatar/chuck-norris.png",
      "id": "73fgwax6s6eukfwgiuqrxq",
      "updated_at": "2020-01-05 13:42:19.324003",
      "url": "https://api.chucknorris.io/jokes/73fgwax6s6eukfwgiuqrxq",
      "value": "Chuck Norris doesn't use GUI, he prefers COMMAND line."
    }"""
    MockServerClient("localhost", 1080)
      .`when`(
          HttpRequest
            .request()
            .withMethod("GET")
            .withPath("/jokes/random")
            .withQueryStringParameter("category", "dev")
      )
      .respond(
          HttpResponse
            .response()
            .withStatusCode(200)
            .withContentType(MediaType.APPLICATION_JSON)
            .withBody(json)
      )
  }

  test("response is retrieved from web client") {
    every { config.host } returns "http://localhost:1080"

    val result = sut.random()

    result.shouldBe("Chuck Norris doesn't use GUI, he prefers COMMAND line.")
  }
})