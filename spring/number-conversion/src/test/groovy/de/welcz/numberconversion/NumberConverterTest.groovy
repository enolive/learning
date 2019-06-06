package de.welcz.numberconversion

import spock.lang.Specification
import spock.lang.Unroll

import static org.assertj.core.api.Assertions.assertThat

class NumberConverterTest extends Specification {
    @Unroll
    def "number #input is converted to #expected"() {
        given: 'a converter'
        NumberConverter converter = new NumberConverter()
        when: 'number is converted'
        BigDecimal result = converter.toBigDecimal(input)
        then: 'the result should match the expected value'
        assertThat(result).isEqualTo(expected)
        where:
        input     | expected
        '0,00'    | '0.00'
        '0'       | '0.00'
        '1'       | '1.00'
        '5,67'    | '5.67'
        '5,675'   | '5.68'
        '5,674'   | '5.67'
        null      | null
        ''        | null
        'invalid' | null
    }
}
