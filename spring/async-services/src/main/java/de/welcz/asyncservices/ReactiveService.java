package de.welcz.asyncservices;

import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.text.MessageFormat;
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
}
