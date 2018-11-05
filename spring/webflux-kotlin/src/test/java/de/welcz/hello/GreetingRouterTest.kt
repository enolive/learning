package de.welcz.hello

import org.assertj.core.api.Assertions.assertThat
import org.junit.Test
import org.junit.runner.RunWith
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.http.MediaType
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.test.web.reactive.server.expectBody

@AutoConfigureWebTestClient
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@RunWith(SpringRunner::class)
class GreetingRouterTest {
    @Autowired
    internal lateinit var client: WebTestClient

    @Test
    fun outputsHelloWithSpecifiedName() {
        client.get()
                .uri("/hello/Spring")
                .accept(MediaType.TEXT_PLAIN)
                .exchange()
                .expectStatus().isOk
                .expectBody<String>()
                .consumeWith { result -> assertThat(result.responseBody).startsWith("Hello, Spring!") }
    }

    @Test
    fun outputsHelloWithDefault() {
        client.get()
                .uri("/hello")
                .accept(MediaType.TEXT_PLAIN)
                .exchange()
                .expectStatus().isOk
                .expectBody<String>()
                .consumeWith { result -> assertThat(result.responseBody).startsWith("Hello, World!") }
    }
}
