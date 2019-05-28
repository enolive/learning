package de.welcz.asyncservices;

import org.junit.Test;

import java.util.concurrent.ExecutionException;

import static org.assertj.core.api.Assertions.assertThat;

public class ReactiveServiceTest {
  @Test
  public void sayHelloShouldWork() throws ExecutionException, InterruptedException {
    ReactiveService service = new ReactiveService();
    String result = service.sayHelloToAsync("World").get();
    assertThat(result).isEqualTo("Hello, World!");
  }
}
