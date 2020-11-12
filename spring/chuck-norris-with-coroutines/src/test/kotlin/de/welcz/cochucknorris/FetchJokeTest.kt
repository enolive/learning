package de.welcz.cochucknorris

import com.ninjasquad.springmockk.MockkBean
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.mockk.coEvery
import io.mockk.every
import io.mockk.mockk
import io.mockk.verify
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ContextConfiguration
import org.springframework.web.reactive.function.client.WebClient
import org.springframework.web.reactive.function.client.awaitBody
import org.springframework.web.reactive.function.client.bodyToMono
import reactor.kotlin.core.publisher.toMono

@SpringBootTest
@ContextConfiguration(classes = [FetchJoke::class, FetchConfig::class])
class FetchJokeTest(
    private val sut: FetchJoke,
    private val config: FetchConfig,
    @MockkBean
    private val builder: WebClient.Builder,
) : FunSpec({
  val webClient = mockk<WebClient>()
  val response = mockk<WebClient.ResponseSpec>()
  val spec = mockk<WebClient.RequestHeadersUriSpec<*>>()

  beforeAny {
    every { builder.baseUrl(any()) } returns builder
    every { builder.build() } returns webClient
    every { webClient.get() } returns spec
    every { spec.uri(any<String>()) } returns spec
    every { spec.retrieve() } returns response
  }

  test("config points to expected host") {
    config.host.shouldBe("https://api.chucknorris.io/")
  }

  test("response is retrieved from web client") {
    // NOTE: can't manage to mock awaitBody. The test will just freeze if I try 😭
    every { response.bodyToMono<FetchJoke.ResponseValue>() } returns
      FetchJoke.ResponseValue("my awesome joke").toMono()

    val result = sut.random()

    result.shouldBe("my awesome joke")
    verify { builder.baseUrl(config.host) }
    verify { spec.uri("/jokes/random?category=dev") }
  }
})