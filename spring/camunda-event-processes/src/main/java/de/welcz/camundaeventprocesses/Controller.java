package de.welcz.camundaeventprocesses;

import io.vavr.collection.HashMap;
import lombok.extern.slf4j.Slf4j;
import org.camunda.bpm.engine.RuntimeService;
import org.camunda.bpm.engine.runtime.EventSubscription;
import org.camunda.bpm.engine.runtime.Execution;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.SecureRandom;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

@RestController
@Slf4j
public class Controller {
  private final RuntimeService runtimeService;

  @Autowired
  public Controller(RuntimeService runtimeService) {
    this.runtimeService = runtimeService;
  }

  @PostMapping("/hellos/{key}")
  public void startProcess(@PathVariable String key) {
    runtimeService.startProcessInstanceByKey("HelloWorldProcess", key);
  }

  @DeleteMapping("/hellos/{key}")
  public void exitProcess(@PathVariable String key) {
    runtimeService.correlateMessage("ExitMessage", key);
  }

  @PostMapping("/hellos/{key}/greetings")
  public int startReceivingHellos(@PathVariable String key) {
    final var processId = randomNumber();
    runtimeService.createMessageCorrelation("ReceiveHellosMessage")
                  .processInstanceBusinessKey(key)
                  .setVariable("sub-process", processId)
                  .correlateAll();
    return processId;
  }

  @PostMapping("/hellos/{key}/greetings/names/{name}")
  public String showGreeting(@PathVariable String key, @PathVariable String name) {
    final var results = runtimeService.createMessageCorrelation("NameMessage")
                                      .processInstanceBusinessKey(key)
                                      .setVariable("name", name)
                                      .correlateAllWithResult();
    return MessageFormat.format("Greetings shown to {0}", results.size());
  }

  @PostMapping("/hellos/{key}/greetings/{subProcess}/names/{name}")
  public void showGreetingsForSpecificProcess(@PathVariable String key,
                                              @PathVariable int subProcess,
                                              @PathVariable String name) {

    findProcessInstanceIdBy(key).flatMap(findSubscriptionToNameMessage(subProcess))
                                .doOnNext(sendNameMessage(name))
                                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                                .block();
  }

  private Function<String, Mono<EventSubscription>> findSubscriptionToNameMessage(int subProcess) {
    return id -> Flux.fromIterable(runtimeService.createEventSubscriptionQuery()
                                                 .processInstanceId(id)
                                                 .eventName("NameMessage")
                                                 .list())
                     .filter(hasVariableWithValue("sub-process-local", subProcess))
                     .next();
  }

  private Mono<String> findProcessInstanceIdBy(String key) {
    return Flux.fromIterable(runtimeService.createProcessInstanceQuery()
                                           .processInstanceBusinessKey(key)
                                           .list())
               .map(Execution::getProcessInstanceId)
               .next();
  }

  private Predicate<EventSubscription> hasVariableWithValue(
      @SuppressWarnings("SameParameterValue") String variableName,
      Object expectedValue) {
    return subscription -> Objects.equals(
        runtimeService.getVariable(subscription.getExecutionId(), variableName),
        expectedValue);
  }

  private Consumer<EventSubscription> sendNameMessage(String name) {
    return subscription -> runtimeService.messageEventReceived("NameMessage",
                                                               subscription.getExecutionId(),
                                                               nameAsMap(name));
  }

  private Map<String, Object> nameAsMap(String name) {
    return HashMap.<String, Object>of("name", name).toJavaMap();
  }

  private int randomNumber() {
    return new SecureRandom().nextInt(100);
  }
}
