package de.welcz.camundawebflux

import org.camunda.bpm.engine.RuntimeService
import org.camunda.bpm.engine.delegate.DelegateExecution
import org.spockframework.spring.SpringBean
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ContextConfiguration
import org.springframework.web.reactive.function.client.WebClient
import reactor.core.publisher.Mono
import spock.lang.Specification

import static de.welcz.camundawebflux.ObjectMother.aString
import static de.welcz.camundawebflux.ProcessConstants.*

@SpringBootTest
@ContextConfiguration(classes = ReadJoke)
class ReadJokeTest extends Specification {
  @Autowired
  private ReadJoke systemUnderTest
  @SpringBean
  private WebClient.Builder builder = Mock()
  private WebClient webClient = Mock()

  def "execution works"() {
    given: "an execution"
    def execution = Mock(DelegateExecution)
    and: "an expected joke"
    def expectedJoke = aString()
    and: "a web client call chain"
    def webClientChain = Mock(WebClient.RequestBodyUriSpec)
    def response = Mock(WebClient.ResponseSpec)

    when: "step is executed"
    systemUnderTest.execute(execution)

    then: "no exception is thrown"
    noExceptionThrown()
    and: "web client was called"
    1 * builder.baseUrl("http://api.icndb.com/jokes/random") >> builder
    1 * builder.build() >> webClient
    1 * webClient.get() >> webClientChain
    1 * webClientChain.retrieve() >> response
    1 * response.bodyToMono(ReadJoke.Joke) >> Mono.just(expectedJoke)
                                                  .map { new ReadJoke.JokeValue(it) }
                                                  .map { new ReadJoke.Joke(it) }
    and: "the joke was memorized in the process"
    1 * execution.setVariable(VARIABLE_JOKE, expectedJoke)
  }
}
