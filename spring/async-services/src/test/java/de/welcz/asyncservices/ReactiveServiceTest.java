package de.welcz.asyncservices;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.concurrent.ExecutionException;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@RunWith(SpringRunner.class)
public class ReactiveServiceTest {
  @Autowired
  private ReactiveService service;

  @Test
  public void sayHelloShouldWork() throws ExecutionException, InterruptedException {
    String result = service.sayHelloToAsync("World").get();
    assertThat(result).isEqualTo("Hello, World!");
  }
}
