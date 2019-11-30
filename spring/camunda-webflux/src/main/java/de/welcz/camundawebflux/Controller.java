package de.welcz.camundawebflux;

import org.camunda.bpm.engine.RuntimeService;
import org.camunda.bpm.engine.exception.NullValueException;
import org.camunda.bpm.engine.runtime.Execution;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;

import java.util.Map;

import static io.vavr.Function1.constant;
import static org.springframework.http.ResponseEntity.created;
import static org.springframework.http.ResponseEntity.noContent;

@RestController
@RequestMapping("/api/v1/chuck-norris")
public class Controller {
  private final RuntimeService runtimeService;

  @Autowired
  public Controller(RuntimeService runtimeService) {
    this.runtimeService = runtimeService;
  }

  @PostMapping("/jokes")
  public Mono<ResponseEntity<String>> start() {
    return Mono.just(runtimeService.startProcessInstanceByKey("ChuckNorrisProcess"))
               .map(Execution::getProcessInstanceId)
               .map(Controller::createdResponse);
  }

  @GetMapping("/jokes/{processInstanceId}")
  public Mono<ResponseEntity<String>> getJoke(@PathVariable String processInstanceId) {
    return Mono.just(processInstanceId)
               .map(id -> runtimeService.getVariable(id, "joke"))
               .doOnNext(id -> runtimeService.createMessageCorrelation(ProcessConstants.MESSAGE_REQUEST_JOKE)
                                             .processInstanceId(processInstanceId)
                                             .correlate())
               .cast(String.class)
               .map(ResponseEntity::ok)
               .onErrorResume(NullValueException.class, constant(Mono.just(noContent().build())));
  }

  private static ResponseEntity<String> createdResponse(String id) {
    return created(UriComponentsBuilder.fromUriString("/jokes/{id}")
                                       .build(Map.of("id", id)))
        .build();
  }
}
