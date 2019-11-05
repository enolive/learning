package de.welcz.camundaeventprocesses;

import org.camunda.bpm.engine.RuntimeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.SecureRandom;

@RestController
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
                  .correlate();
    return processId;
  }

  @PostMapping("/hellos/{key}/greetings/{name}")
  public void showGreeting(@PathVariable String key, @PathVariable String name) {
    runtimeService.createMessageCorrelation("NameMessage")
                  .processInstanceBusinessKey(key)
                  .setVariable("name", name)
                  .correlateAll();
  }

  private int randomNumber() {
    return new SecureRandom().nextInt(100);
  }
}
