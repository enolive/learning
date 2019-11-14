package de.welcz.camundawebflux;

import org.camunda.bpm.engine.RuntimeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/rest-api/v1/chuck-norris")
public class Controller {
  private final RuntimeService runtimeService;

  @Autowired
  public Controller(RuntimeService runtimeService) {
    this.runtimeService = runtimeService;
  }

  @PostMapping("/jokes")
  public String start() {
    return runtimeService.startProcessInstanceByKey("ChuckNorrisProcess")
                         .getProcessInstanceId();
  }
}
