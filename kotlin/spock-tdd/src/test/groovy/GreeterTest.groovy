import groovy.transform.CompileDynamic
import spock.lang.Specification

@CompileDynamic
class GreeterTest extends Specification {
    def "not"() {
        expect:
        true == false
    }

    def "yeah"() {
        expect:
        true == true
    }
}
