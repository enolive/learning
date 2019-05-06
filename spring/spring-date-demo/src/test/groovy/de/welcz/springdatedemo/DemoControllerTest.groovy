package de.welcz.springdatedemo

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.mock.web.MockHttpServletResponse
import org.springframework.test.web.servlet.MockMvc
import spock.lang.Specification

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Month

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get

@WebMvcTest(DemoController)
class DemoControllerTest extends Specification {
    @Autowired
    private MockMvc mockMvc

    def "get data for specific date should work"() {
        given: 'a date'
        String date = LocalDate.now().toString()
        when: 'get request is performed'
        MockHttpServletResponse response = mockMvc.perform(get("/api/v1/date/$date"))
                                                  .andReturn().response
        then: 'the response status is OK'
        response.status == HttpStatus.OK.value()
        and: 'the response should contain the date'
        response.contentAsString == "Hello, $date!"
    }

    def "get data for specific date time should work"() {
        given: 'a date time'
        String dateTime = LocalDateTime.now().toString()
        when: 'get request is performed'
        MockHttpServletResponse response = mockMvc.perform(get("/api/v1/datetime/$dateTime"))
                                                  .andReturn().response
        then: 'the response status is OK'
        response.status == HttpStatus.OK.value()
        and: 'the response should contain the date'
        response.contentAsString == "Hello, $dateTime!"
    }

    def "get data for basic iso date should work"() {
        given: 'a date'
        String basicDate = "20190101"
        String expectedDate = LocalDate.of(2019, Month.JANUARY, 1)
        when: 'get request is performed'
        MockHttpServletResponse response = mockMvc.perform(get("/api/v1/basicdate/$basicDate"))
                                                  .andReturn().response
        then: 'the response status is OK'
        response.status == HttpStatus.OK.value()
        and: 'the response should contain the date'
        response.contentAsString == "Hello, $expectedDate!"
    }

    def "post data with json object containing a date time should work"() {
        given: 'a date'
        String json = '{"date": "2019-06-23T19:23:12.123+0100", "name": "Christoph"}'
        when: 'get request is performed'
        MockHttpServletResponse response = mockMvc
                .perform(get("/api/v1/json")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json))
                .andReturn().response

        then: 'the response status is OK'
        response.status == HttpStatus.OK.value()
        and: 'the response should contain the date'
        response.contentAsString == 'Hello, Christoph 2019-06-23 19:23!'
    }
}
