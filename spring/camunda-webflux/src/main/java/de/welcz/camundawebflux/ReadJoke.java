package de.welcz.camundawebflux;

import lombok.Value;
import org.camunda.bpm.engine.delegate.DelegateExecution;
import org.camunda.bpm.engine.delegate.JavaDelegate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.util.function.Consumer;

@Service
public class ReadJoke implements JavaDelegate {
  private final WebClient.Builder builder;

  @Autowired
  public ReadJoke(WebClient.Builder builder) {
    this.builder = builder;
  }

  @Override
  public void execute(DelegateExecution execution) {
    retrieveJoke()
        .doOnNext(storeInProcessVariables(execution))
        .block();
  }

  private Mono<String> retrieveJoke() {
    // this works
    // if (true) return Mono.just("test");
    // this will produce a serializer error ENGINE-03041 :-(
    return builder.baseUrl("http://api.icndb.com/jokes/random")
                  .build()
                  .get()
                  .retrieve()
                  .bodyToMono(Joke.class)
                  .map(Joke::getValue)
                  .map(JokeValue::getJoke)
                  .log();
  }

  private Consumer<String> storeInProcessVariables(DelegateExecution execution) {
    return joke -> execution.setVariable("jokeText", joke);
  }

  @Value
  private static class Joke {
    JokeValue value;
  }

  @Value
  private static class JokeValue {
    String joke;
  }
}
