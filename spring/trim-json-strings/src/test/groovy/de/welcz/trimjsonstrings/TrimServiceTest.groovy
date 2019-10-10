package de.welcz.trimjsonstrings

import groovy.json.JsonSlurper
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.json.JsonTest
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.annotation.Import
import org.springframework.test.context.ContextConfiguration
import spock.lang.Shared
import spock.lang.Specification

@JsonTest
@Import(TrimmingModule)
@ContextConfiguration(classes = TrimService)
class TrimServiceTest extends Specification {
    @Autowired
    private TrimService service
    @Shared
    private JsonSlurper slurper = new JsonSlurper()

    def "simple json gets trimmed"() {
        given: 'a json with whitespaces'
        String inputJson = """
        {
          "first": "  hello   "
        }
        """
        and: 'an expected json'
        String expectedJson = """
        {
          "first": "hello"
        }
        """

        when: 'json is converted'
        String result = service.trim(inputJson)

        then: 'the expected json is returned'
        slurper.parseText(result) == slurper.parseText(expectedJson)
    }

    def "complex json gets trimmed"() {
        given: 'a json with whitespaces'
        String inputJson = """
        {
          "first": "  hello   ",
          "nullValue": null,
          "number": 123.50,
          "boolean": true,
          "nested": {
            "value": "   more blanks   "
          }
        }
        """
        and: 'an expected json'
        String expectedJson = """
        {
          "first": "hello",
          "nullValue": null,
          "number": 123.50,
          "boolean": true,
          "nested": {
            "value": "more blanks"
          }
        }
        """

        when: 'json is converted'
        String result = service.trim(inputJson)

        then: 'the expected json is returned'
        slurper.parseText(result) == slurper.parseText(expectedJson)
    }
}
