import groovy.json.JsonOutput
import spock.lang.Specification
import spock.lang.Unroll

class ResultTest extends Specification {
    @Unroll
    def "sequence with #errors, #successes should be (#expectedErrors, #expectedItems) because #because"() {
        given: 'a sequence'
        List<Result> givenItems = successes.collect { Result.success(it) }
        List<Result> givenErrors = errors.collect { Result.error(it) }
        List<Result> sequence = givenItems + givenErrors
        when: 'result sequence is generated'
        def result = Result.sequence(sequence)
        then: 'result should contain errors and successes'
        result.error == expectedErrors
        result.success == expectedItems
        where:
        errors   | successes               | because              | expectedErrors | expectedItems
        []       | []                      | 'both are empty'     | []             | []
        []       | ['Hello']               | 'one success'        | []             | ['Hello']
        []       | ['Hello', 'World', '!'] | 'multiple successes' | []             | ['Hello', 'World', '!']
        [42]     | []                      | 'one error'          | [42]           | []
        [42, 23] | []                      | 'multiple errors'    | [42, 23]       | []
    }

    @Unroll
    def "list of success lists with #successes should be flattened to #expectedSuccesses because #because"() {
        given: 'a result with list of successes'
        def result = Result.success(successes)
        when: 'result is flattened'
        def flattened = Result.flattenSuccess(result)
        then: 'result should flatten a list of list of successes'
        flattened.success == expectedSuccesses
        where:
        successes                   | because          | expectedSuccesses
        []                          | 'empty list'     | []
        [['Hello']]                 | 'single list'    | ['Hello']
        [['Hello', 'World'], ['!']] | 'multiple lists' | ['Hello', 'World', '!']
    }

    def "flattening an error should keep it structure"() {
        given: 'a result with an error'
        Result<String, List<?>> result = Result.error('OH NO!!!!1')
        when: 'result is flattened'
        def flattened = Result.flattenSuccess(result)
        then: 'result should stay the same'
        JsonOutput.toJson(flattened) == JsonOutput.toJson(result)
    }

    def "flattening a sequence should keep its errors as they were"() {
        given: 'a result from a sequence'
        def sequence = [Result.error('OH'), Result.success([23, 42]), Result.error('NO'), Result.success([11])]
        Result<?, List<?>> result = Result.sequence(sequence)
        when: 'result is flattened'
        def flattened = Result.flattenSuccess(result)
        then: 'error should stay the same'
        flattened.error == ['OH', 'NO']
        and: 'success is still flattened'
        flattened.success == [23, 42, 11]
    }

    def "flatten a null should fail"() {
        when: 'null is flattened'
        Result.flattenSuccess(null)
        then: 'exception is thrown'
        def ex = thrown(NullPointerException)
        ex.message == 'result must not be null.'
    }

    def "sequencing a null should fail"() {
        when: 'null is sequenced'
        Result.sequence(null)
        then: 'exception is thrown'
        def ex = thrown(NullPointerException)
        ex.message == 'results must not be null.'
    }

    def "creating a success with null should fail"() {
        when: 'result without an success is created'
        Result.success(null)
        then: 'exception is thrown'
        def ex = thrown(NullPointerException)
        ex.message == 'success must not be null.'
    }

    def "creating an error with null should fail"() {
        when: 'result without an error is created'
        Result.error(null)
        then: 'exception is thrown'
        def ex = thrown(NullPointerException)
        ex.message == 'error must not be null.'
    }
}
