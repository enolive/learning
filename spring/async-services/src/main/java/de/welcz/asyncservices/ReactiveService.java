package de.welcz.asyncservices;

import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.text.MessageFormat;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;

@Service
public class ReactiveService {
  @Async
  public CompletableFuture<String> sayHelloToAsync(String name) {
    try {
      Thread.sleep(2_000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
    return CompletableFuture.completedFuture(MessageFormat.format("Hello, {0}!", name));
  }

  Mono<String> sayHelloToReactive(String name) {
    if (true) return Mono.fromCallable(() -> longSayHello(name));
    return Mono.delay(Duration.ofSeconds(2))
               .thenReturn(MessageFormat.format("Hello, {0}!", name));
  }

  private String longSayHello(String name) {
    try {
      Thread.sleep(2_000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
    return MessageFormat.format("Hello, {0}!", name);
  }
}
