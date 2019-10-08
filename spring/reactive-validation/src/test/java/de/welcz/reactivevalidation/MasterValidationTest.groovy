package de.welcz.reactivevalidation

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.validation.ValidationAutoConfiguration
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ContextConfiguration
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean
import spock.lang.Specification

import javax.validation.ConstraintViolation

@SpringBootTest(classes = ValidationAutoConfiguration)
@ContextConfiguration(classes = [])
class MasterValidationTest extends Specification {
    @Autowired
    private LocalValidatorFactoryBean validator

    def "validation works"() {
        given: 'an invalid object'
        Master master = new Master(null, null)

        when: 'object is validated by hand'
        Set<ConstraintViolation<Master>> results = validator.validate(master)

        then: 'a set of errors is returned'
        results.size() == 2
    }
}
