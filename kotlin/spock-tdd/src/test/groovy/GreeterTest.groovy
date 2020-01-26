import groovy.transform.CompileDynamic
import spock.lang.Specification

@CompileDynamic
class GreeterTest extends Specification {
    def "framework works"() {
        expect:
        true == false
    }
}
