package de.welcz.camundawebflux

import org.camunda.bpm.engine.ActivityTypes
import org.camunda.bpm.engine.HistoryService
import org.camunda.bpm.engine.RuntimeService
import org.camunda.bpm.engine.history.HistoricActivityInstance
import org.camunda.bpm.engine.runtime.ProcessInstance
import org.camunda.bpm.engine.test.Deployment
import org.camunda.bpm.engine.test.ProcessEngineRule
import org.junit.ClassRule
import spock.lang.Shared
import spock.lang.Specification

import static de.welcz.camundawebflux.ProcessConstants.MESSAGE_REQUEST_JOKE
import static de.welcz.camundawebflux.ProcessConstants.PROCESS_DEFINITION_KEY
import static org.camunda.bpm.engine.test.assertions.bpmn.BpmnAwareTests.*
import static org.camunda.bpm.engine.test.mock.Mocks.register

@Deployment(resources = "chuck_norris.bpmn")
class CamundaProcessTest extends Specification {
  @ClassRule
  @Shared
  private ProcessEngineRule processEngineRule = new ProcessEngineRule()
  private RuntimeService runtimeService
  private HistoryService historyService
  private ReadJoke readJoke = Mock()

  void setup() {
    runtimeService = processEngineRule.runtimeService
    historyService = processEngineRule.historyService
    register("readJoke", readJoke)
  }

  def "process works"() {
    when: "process is started"
    def processInstance = runtimeService.startProcessInstanceByKey(PROCESS_DEFINITION_KEY)

    then: "process runs"
    assertThat(processInstance).isStarted().isWaitingAt("ReadServiceTask")

    when: "joke is read"
    2.times { execute(job()) }

    then: "joke delegate was executed"
    1 * readJoke.execute(_)
    then: "process waits for somebody requesting the joke"
    assertThat(processInstance).isWaitingAt("RequestJokeReceiveTask")

    when: "someone wants to hear the joke"
    runtimeService.createMessageCorrelation(MESSAGE_REQUEST_JOKE)
                  .processInstanceId(processInstance.processInstanceId)
                  .correlate()

    then: "process has passed all steps and ended"
    assertThat(processInstance).hasPassedInOrder("StartEvent",
                                                 "ReadServiceTask",
                                                 "RequestJokeReceiveTask",
                                                 "JokeWasReadEvent",
                                                 "EndEvent")
                               .isEnded()
    and: "activities were remembered in history"
    throwNoneEvents(processInstance)
        .collect { it.activityName } == ["Someone likes my jokes!"]
  }

  def "no one wants the joke :-("() {
    when: "process is started"
    def processInstance = runtimeService.startProcessInstanceByKey(PROCESS_DEFINITION_KEY)

    then: "process is running"
    assertThat(processInstance).isStarted()

    when: "read joke is executed"
    2.times { execute(job()) }
    then: "process waits for someone to request the joke"
    assertThat(processInstance).isWaitingAt("RequestJokeReceiveTask")

    when: "no message arrives before the timeout"
    execute(jobQuery().activityId("WaitForJokeRequestTimeout").singleResult())

    then: "process ends with an error"
    assertThat(processInstance).hasPassedInOrder("WaitForJokeRequestTimeout",
                                                 "NoOneWantsTheJokeEvent",
                                                 "TimeoutErrorEndEvent")
                               .isEnded()
    and: "joke was not read"
    assertThat(processInstance).hasNotPassed("JokeWasReadEvent")
    and: "activities were remembered"
    throwNoneEvents(processInstance).collect { it.activityName } == ["no one wants to hear my jokes :-("]
  }

  private List<HistoricActivityInstance> throwNoneEvents(ProcessInstance processInstance) {
    historyService.createHistoricActivityInstanceQuery()
                  .processInstanceId(processInstance.id)
                  .activityType(ActivityTypes.INTERMEDIATE_EVENT_NONE_THROW)
                  .list()
  }
}
