package de.welcz.springdatedemo

import org.springframework.boot.test.context.SpringBootTest
import spock.lang.Specification

@SpringBootTest
class DemoApplicationTest extends Specification {
    def "context should load"() {
        expect:
        true
    }
}
