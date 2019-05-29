package de.welcz.asyncservices;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;
import reactor.test.publisher.PublisherProbe;
import reactor.test.StepVerifier;

import java.util.concurrent.CompletableFuture;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@WebFluxTest(controllers = ReactiveController.class)
public class ReactiveControllerTest {
  @Autowired
  private WebTestClient testClient;
  @MockBean
  private ReactiveService reactiveService;

  @Test
  public void sayHelloShouldWork() {
    doReturn(CompletableFuture.completedFuture("")).when(reactiveService).sayHelloToAsync(anyString());

    testClient.get()
              .uri("/hello/Christoph")
              .exchange()
              .expectStatus().isOk()
              .expectBody(String.class)
              .consumeWith(content -> assertThat(content.getResponseBody()).isEqualTo("I just said hello."));
    verify(reactiveService, times(1)).sayHelloToAsync("Christoph");
  }

  @Test
  public void sayHelloRShouldWork() {
    var probe = PublisherProbe.empty();
    doReturn(probe.mono()).when(reactiveService).sayHelloToReactive(anyString());

    testClient.get()
              .uri("/helloR/Christoph")
              .exchange()
              .expectStatus().isOk()
              .expectBody(String.class)
              .consumeWith(content -> assertThat(content.getResponseBody()).isEqualTo("I just said hello reactive."));
    verify(reactiveService, times(1)).sayHelloToReactive("Christoph");
    probe.assertWasSubscribed();
  }
}
