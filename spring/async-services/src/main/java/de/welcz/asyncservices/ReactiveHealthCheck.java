package de.welcz.asyncservices;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.ReactiveHealthIndicator;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

@Component(value = "reactive health check")
public class ReactiveHealthCheck implements ReactiveHealthIndicator {
  private final WebClient client;

  @Autowired
  public ReactiveHealthCheck(WebClient.Builder builder) {
    client = builder.baseUrl("http://www.google.de/").build();
  }

  @Override
  public Mono<Health> health() {
    return client.get()
                 .exchange()
                 .map(this::validateResponse)
                 .onErrorResume(throwable -> Mono.just(Health.down().withException(throwable).build()));
  }

  private Health validateResponse(ClientResponse response) {
    return response.statusCode().is2xxSuccessful()
        ? Health.up().build()
        : Health.down()
                .withDetail("status", response.statusCode())
                .build();
  }
}
