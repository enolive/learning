package de.welcz.camundawebflux

import groovy.transform.CompileDynamic
import org.camunda.bpm.engine.RuntimeService
import org.camunda.bpm.engine.exception.NullValueException
import org.camunda.bpm.engine.runtime.MessageCorrelationBuilder
import org.camunda.bpm.engine.runtime.ProcessInstance
import org.spockframework.spring.SpringBean
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.test.web.reactive.server.WebTestClient
import spock.lang.Specification

import static de.welcz.camundawebflux.ProcessConstants.*

@WebFluxTest(controllers = Controller)
@CompileDynamic
class ControllerTest extends Specification {
  @Autowired
  private WebTestClient webTestClient
  @SpringBean
  private RuntimeService runtimeService = Mock()

  def "starting process"() {
    given: "an uri"
    def uri = "/api/v1/chuck-norris/jokes"
    and: "a process instance"
    def processInstance = aProcessInstance()

    when: "api is called"
    def response = webTestClient.post()
                                .uri(uri)
                                .exchange()

    then: "status is created"
    response.expectStatus().isCreated()
    and: "process instance id was returned to the caller"
    response.expectHeader().valueEquals("location", "/jokes/${processInstance.processInstanceId}")
    and: "process was started"
    1 * runtimeService.startProcessInstanceByKey(PROCESS_DEFINITION_KEY) >> processInstance
  }

  def "reading joke is successful"() {
    given: "a process instance id"
    def processInstanceId = ObjectMother.aString()
    and: "an uri"
    def uri = "/api/v1/chuck-norris/jokes/$processInstanceId"
    and: "an expected joke"
    def expectedJoke = ObjectMother.aString()
    and: "a message correlation"
    def correlation = Mock(MessageCorrelationBuilder)

    when: "api is called"
    def response = webTestClient.get()
                                .uri(uri)
                                .exchange()

    then: "status is ok"
    response.expectStatus().isOk()
    and: "body contains the joke"
    response.expectBody(String).isEqualTo(expectedJoke)
    and: "process was asked for message"
    1 * runtimeService.createMessageCorrelation(MESSAGE_REQUEST_JOKE) >> correlation
    1 * correlation.processInstanceId(processInstanceId) >> correlation
    1 * correlation.correlate()
    and: "variable was read"
    1 * runtimeService.getVariable(processInstanceId, VARIABLE_JOKE) >> expectedJoke
  }

  def "reading not existing joke returns nothing"() {
    given: "a process instance id"
    def processInstanceId = ObjectMother.aString()
    and: "an uri"
    def uri = "/api/v1/chuck-norris/jokes/$processInstanceId"

    when: "api is called"
    def response = webTestClient.get()
                                .uri(uri)
                                .exchange()

    then: "status is no content"
    response.expectStatus().isNoContent()
    and: "body is empty"
    response.expectBody().isEmpty()
    and: "variable was attempted to read"
    1 * runtimeService.getVariable(processInstanceId, VARIABLE_JOKE) >> { throw new NullValueException() }
    and: "process was never asked for message"
    0 * runtimeService.createMessageCorrelation(MESSAGE_REQUEST_JOKE)
  }

  private aProcessInstance() {
    Mock(ProcessInstance) {
      it.processInstanceId >> ObjectMother.aString()
    }
  }
}
