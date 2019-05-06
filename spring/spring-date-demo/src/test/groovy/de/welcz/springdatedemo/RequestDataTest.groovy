package de.welcz.springdatedemo

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule
import org.assertj.core.api.Assertions
import spock.lang.Specification

import java.time.LocalDateTime
import java.time.Month

class RequestDataTest extends Specification {
    def "deserialization works"() {
        given: 'a data object'
        RequestData expected = new RequestData(
                'Christoph',
                LocalDateTime.of(2019, Month.JUNE, 23, 19, 23, 12))
        and: 'a JSON'
        String json = '{"date": "2019-06-23T19:23:12.000+0100", "name": "Christoph"}'
        when: 'object is deserialized'
        ObjectMapper mapper = new ObjectMapper()
        mapper.registerModules(new JavaTimeModule(), new ParameterNamesModule(), new Jdk8Module())
        RequestData actual = mapper.readValue(json, RequestData)
        then: 'object is a JSON'
        Assertions.assertThat(actual).isEqualToComparingFieldByFieldRecursively(expected)
    }
}
