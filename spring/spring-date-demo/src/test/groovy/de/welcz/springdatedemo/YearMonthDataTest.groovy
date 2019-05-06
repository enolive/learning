package de.welcz.springdatedemo

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule
import org.assertj.core.api.Assertions
import spock.lang.Specification

import java.time.Month
import java.time.YearMonth

class YearMonthDataTest extends Specification {
    def "deserialization works"() {
        given: 'a data object'
        YearMonthData expected = new YearMonthData("Christoph", YearMonth.of(2019, Month.JANUARY))
        and: 'a JSON'
        String json = '{"name": "Christoph", "date": "201901"}'
        when: 'object is deserialized'
        ObjectMapper mapper = new ObjectMapper()
        mapper.registerModules(new JavaTimeModule(), new ParameterNamesModule(), new Jdk8Module())
        YearMonthData actual = mapper.readValue(json, YearMonthData)
        then: 'object is a JSON'
        Assertions.assertThat(actual).isEqualToComparingFieldByFieldRecursively(expected)
    }
}
