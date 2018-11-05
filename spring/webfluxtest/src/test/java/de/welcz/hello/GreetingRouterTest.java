package de.welcz.hello;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.assertj.core.api.Assertions.assertThat;

@AutoConfigureWebTestClient
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@RunWith(SpringRunner.class)
public class GreetingRouterTest {
    @Autowired
    WebTestClient client;

    @Test
    public void outputsHelloWithSpecifiedName() {
        client.get()
              .uri("/hello/Spring")
              .accept(MediaType.TEXT_PLAIN)
              .exchange()
              .expectStatus().isOk()
              .expectBody(String.class)
              .consumeWith(result -> assertThat(result.getResponseBody()).startsWith("Hello, Spring!"));
    }

    @Test
    public void outputsHelloWithDefault() {
        client.get()
              .uri("/hello")
              .accept(MediaType.TEXT_PLAIN)
              .exchange()
              .expectStatus().isOk()
              .expectBody(String.class)
              .consumeWith(result -> assertThat(result.getResponseBody()).startsWith("Hello, World!"));
    }
}
