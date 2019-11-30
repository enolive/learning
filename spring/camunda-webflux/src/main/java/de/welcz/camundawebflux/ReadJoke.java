package de.welcz.camundawebflux;

import lombok.Value;
import org.camunda.bpm.engine.delegate.DelegateExecution;
import org.camunda.bpm.engine.delegate.JavaDelegate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import static de.welcz.camundawebflux.ProcessConstants.VARIABLE_JOKE;

@Service
public class ReadJoke implements JavaDelegate {
  private final WebClient.Builder builder;

  @Autowired
  public ReadJoke(WebClient.Builder builder) {
    this.builder = builder;
  }

  @Override
  public void execute(DelegateExecution execution) {
    // XXX: don't trigger the execution inside a reactor thread!
    //  this will fail!
    final var jokeText = retrieveJoke().block();
    execution.setVariable(VARIABLE_JOKE, jokeText);
  }

  private Mono<String> retrieveJoke() {
    return builder.baseUrl("http://api.icndb.com/jokes/random")
                  .build()
                  .get()
                  .retrieve()
                  .bodyToMono(Joke.class)
                  .map(Joke::getValue)
                  .map(JokeValue::getJoke)
                  .log();
  }

  @Value
  static class Joke {
    JokeValue value;
  }

  @Value
  static class JokeValue {
    String joke;
  }
}
