package de.welcz.camundawebflux;

import lombok.extern.slf4j.Slf4j;
import org.camunda.bpm.engine.delegate.DelegateExecution;
import org.camunda.bpm.engine.delegate.JavaDelegate;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class OutputJoke implements JavaDelegate {
  @Override
  public void execute(DelegateExecution execution) {
    LOGGER.info("here's a fact about Chuck Norris: {}", execution.getVariable("jokeText"));
  }
}
