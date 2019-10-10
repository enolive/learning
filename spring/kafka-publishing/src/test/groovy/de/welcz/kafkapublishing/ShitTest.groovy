package de.welcz.kafkapublishing

import com.fasterxml.jackson.databind.ObjectMapper
import groovy.json.JsonSlurper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.json.JsonTest
import spock.lang.Shared
import spock.lang.Specification

import java.time.YearMonth

@JsonTest
class ShitTest extends Specification {
    @Autowired
    private ObjectMapper mapper
    @Shared
    private JsonSlurper slurper = new JsonSlurper()

    def "shit can be serialized"() {
        given: 'some shit'
        Shit shit = new Shit('12345-6789', YearMonth.of(2019, 5))
        and: 'expected json'
        String expected = """
        {
            "clientId": "12345-6789",
            "date": "2019-05"
        }
        """

        when: 'object is serialized'
        String result = mapper.writeValueAsString(shit)

        then: 'expected json is generated'
        slurper.parseText(result) == slurper.parseText(expected)
    }

    def "shit can be deserialized"() {
        given: 'some json'
        String json = """
        {
            "clientId": "12345-6789",
            "date": "2019-05"
        }
        """
        and: 'expected object'
        Shit expected = new Shit('12345-6789', YearMonth.of(2019, 5))

        when: 'object is deserialized'
        Shit result = mapper.readValue(json, Shit)

        then: 'expected object is generated'
        result == expected
    }
}
