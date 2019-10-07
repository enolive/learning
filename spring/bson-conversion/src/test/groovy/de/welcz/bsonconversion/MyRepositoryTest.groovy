package de.welcz.bsonconversion

import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest
import spock.lang.Specification

import java.time.YearMonth
import java.time.ZonedDateTime

@DataMongoTest
class RepositoryTest extends Specification {
    Repository repository

    def "document can be saved"() {
        given: 'a document'
        MyDocument document = new MyDocument(ZonedDateTime.now(), YearMonth.now())

        when: 'document is saved'


        then: 'saved document is returned'
    }
}
