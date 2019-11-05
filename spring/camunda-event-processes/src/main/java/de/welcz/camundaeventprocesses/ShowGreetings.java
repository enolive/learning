package de.welcz.camundaeventprocesses;

import lombok.extern.slf4j.Slf4j;
import org.camunda.bpm.engine.delegate.DelegateExecution;
import org.camunda.bpm.engine.delegate.JavaDelegate;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ShowGreetings implements JavaDelegate {
  @Override
  public void execute(DelegateExecution delegateExecution) {
    LOGGER.info("Hello, {} from {}!",
                delegateExecution.getVariable("name"),
                delegateExecution.getVariableLocal("sub-process-local"));
  }
}
