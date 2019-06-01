package de.welcz.numberconversion

import org.spockframework.spring.SpringBean
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.test.web.reactive.server.WebTestClient
import spock.lang.Specification

import static org.assertj.core.api.Assertions.assertThat

@WebFluxTest(controllers = NumberController)
class NumberControllerTest extends Specification {
    @Autowired
    private WebTestClient testClient

    @SpringBean
    private NumberConverter converter = Mock(NumberConverter)

    def "converting number should work"() {
        given: 'a number'
        String number = 'abc'
        and: 'a result'
        BigDecimal result = 123.45

        when: 'GET call is performed'
        WebTestClient.ResponseSpec response = testClient.get()
                                                        .uri('/number/{number}', number)
                                                        .exchange()
        then: 'the response is the expected value'
        response.expectStatus().isOk()
                .expectBody(String)
                .consumeWith { assertThat(it.responseBody).isEqualTo(result.toString()) }
        and: 'converter is called'
        1 * converter.toBigDecimal(number) >> result
    }
}
